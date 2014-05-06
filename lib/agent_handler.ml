(*
 * Copyright (c) 2014 David Sheets <sheets@alum.mit.edu>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

(* This module manages a network of asset identities as subprocesses. *)

(* WARNING: DOES NOT PROVIDE ISOLATION!
   If an agent is compromised, it can compromise the handler and the handler
   can compromise the parent.
   WARNING: DOES NOT PROVIDE ISOLATION!
 *)

type 'a result =
| Ok of 'a
| Unix_error of Unix.error * string * string
| Err of exn
type _ command =
| Access : string * Unix.access_permission list -> unit command
| Chown  : string * int32 * int32 -> unit command
type 'a directive = {
  uid : int32;
  gid : int32;
  cmd : 'a command;
}
type 'a interp = uid:int32 -> gid:int32 -> 'a command -> 'a result
type 'a perspective = uid:int32 -> gid:int32 -> 'a
type interp_box = { interp : 'a. 'a interp }
type t = {
  access : (string -> Unix.access_permission list -> unit) perspective;
  chown  : (string -> int32 -> int32 -> unit) perspective;
}

let unwrap_result = function
  | Ok retval -> retval
  | Unix_error (err, call, arg) -> raise (Unix.Unix_error (err, call, arg))
  | Err exn -> raise exn

let make box = let { interp } = box in {
  access = (fun ~uid ~gid path perms ->
    unwrap_result (interp ~uid ~gid (Access (path, perms))));
  chown = (fun ~uid ~gid path u g ->
    unwrap_result (interp ~uid ~gid (Chown (path, u, g))));
}

let zero = make {
  interp = (fun ~uid ~gid _cmd -> raise (Failure "Agent handler not started"));
}

let fork_proc listen request =
  let request_in, request_out = Unix.pipe () in
  let reply_in, reply_out = Unix.pipe () in
  let agent_handler = Unix.fork () in
  if agent_handler = 0
  then listen
    (Unix.in_channel_of_descr request_in)
    (Unix.out_channel_of_descr reply_out)
  else request
    (Unix.out_channel_of_descr request_out)
    (Unix.in_channel_of_descr reply_in)

let agent_request (type a) request reply (cmd : a command) : a result =
  Marshal.to_channel request cmd [];
  flush request;
  ((Marshal.from_channel reply) : a result)

let reply (type a) : a command -> a result = fun cmd ->
  try Ok (match cmd with
  | Access (path,perms) -> Unix.access path perms
  | Chown (path, uid, gid) ->
    Unix.chown path (Int32.to_int uid) (Int32.to_int gid)
  ) with
  | Unix.Unix_error (err, call, arg) -> Unix_error (err, call, arg)
  | exn -> Err exn

let create_agent uid gid =
  fork_proc (fun (type a) requestc replyc ->
    Unix.setgid (Int32.to_int gid);
    Unix.setuid (Int32.to_int uid);
    while true do
      Marshal.to_channel replyc
        ((reply ((Marshal.from_channel requestc) : a command)) : a result) [];
      flush replyc
    done;
    raise Exit
  ) agent_request

let listen (type a) request reply =
  let agents = Hashtbl.create 8 in
  while true do
    let directive : a directive = Marshal.from_channel request in
    let { uid; gid; cmd } = directive in
    let agent =
      try
        Hashtbl.find agents (uid,gid)
      with Not_found ->
        let a = create_agent uid gid in
        Hashtbl.replace agents (uid,gid) a;
        a
    in
    Marshal.to_channel reply (agent cmd) [];
    flush reply
  done;
  raise Exit

let request request reply = {
  interp = (fun (type a) ~uid ~gid (cmd : a command) ->
    Marshal.to_channel request { uid; gid; cmd } [];
    flush request;
    ((Marshal.from_channel reply) : a result)
  )
}

let create () = make (fork_proc listen request)