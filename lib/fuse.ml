(*
 * Copyright (c) 2014-2015 David Sheets <sheets@alum.mit.edu>
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

module Nodes = Nodes

module In = Profuse.In

type 'a structure = 'a Ctypes_static.structure
type request = In.Message.t Profuse.request

module type BASE_IO = sig
  type 'a t

  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t

  val return : 'a -> 'a t

  val fail : exn -> 'a t

  val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t
end

module type IO = sig
  include BASE_IO

  module In : sig
    val read : Profuse.chan -> unit -> request t
  end

  module Out : sig
    val write_reply :
      'a Profuse.request -> ('a Profuse.request -> char Ctypes.CArray.t)
      -> unit t
    val write_ack : request -> unit t
    val write_error : (string -> unit) -> request -> Errno.t -> unit t
  end
end

module type RO_SIMPLE = sig
  module IO : IO
  type 'a req_handler = request -> 'a -> 'a IO.t
  type t

  val getattr    : t req_handler
  val opendir    : In.Open.T.t structure -> t req_handler
  val forget     : Unsigned.UInt64.t -> t req_handler
  val lookup     : string -> t req_handler
  val readdir    : In.Read.T.t structure -> t req_handler
  val readlink   : t req_handler
  val release    : In.Release.T.t structure -> t req_handler
  val releasedir : In.Release.T.t structure -> t req_handler
  val open_      : In.Open.T.t structure -> t req_handler
  val read       : In.Read.T.t structure -> t req_handler
  val access     : In.Access.T.t structure -> t req_handler
  val destroy    : t req_handler
end

module type RO_MID = sig
  include RO_SIMPLE

  val statfs : t req_handler
end

module type RO_FULL = sig
  include RO_MID

  val getxattr  : In.Getxattr.T.t structure -> string -> t req_handler
  val listxattr : In.Getxattr.T.t structure -> t req_handler
  val interrupt : In.Interrupt.T.t structure -> t req_handler
  val bmap      : In.Bmap.T.t structure -> t req_handler
end

module type RW_SIMPLE = sig
  include RO_SIMPLE

  val flush   : In.Flush.T.t structure -> t req_handler
  val link    : In.Link.T.t structure -> string -> t req_handler
  val symlink : string -> string -> t req_handler
  val rename  : In.Rename.T.t structure -> string -> string -> t req_handler
  val unlink  : string -> t req_handler
  val rmdir   : string -> t req_handler
  val mknod   : In.Mknod.T.t structure -> string -> t req_handler
  val write   : In.Write.T.t structure -> char Ctypes.ptr -> t req_handler
  val mkdir   : In.Mkdir.T.t structure -> string -> t req_handler
  val setattr : In.Setattr.T.t structure -> t req_handler
end

module type RW_MID = sig
  include RO_MID
  include RW_SIMPLE
    with module IO := IO
     and type t := t
     and type 'a req_handler := 'a req_handler

  val create      : In.Create.T.t structure -> string -> t req_handler
  val fsync       : In.Fsync.T.t structure -> t req_handler
end

module type RW_FULL = sig
  include RO_FULL
  include RW_MID
    with module IO := IO
     and type t := t
     and type 'a req_handler := 'a req_handler

  val fsyncdir    : In.Fsync.T.t structure -> t req_handler
  val setxattr    : In.Setxattr.T.t structure -> string -> t req_handler
  val removexattr : string -> t req_handler
  val getlk       : In.Lk.T.t structure -> t req_handler (* TODO: RO? *)
  val setlk       : In.Lk.T.t structure -> t req_handler (* TODO: RO? *)
  val setlkw      : In.Lk.T.t structure -> t req_handler (* TODO: RO? *)
end

module type FS_IO = sig
  include RW_FULL

  val negotiate_mount :
    In.Init.T.t structure -> request -> t -> (request * t) IO.t

  val dispatch : request -> t -> t IO.t
end

module type STATE = sig
  type t

  val string_of_nodeid : Unsigned.UInt64.t -> t -> string

  val string_of_state : request -> t -> string
end

module type FS = sig
  include STATE

  module Calls :
    functor(IO : IO) -> FS_IO with type 'a IO.t = 'a IO.t and type t = t
end

module Zero(State : STATE)(IO : IO)
  : RW_FULL with module IO = IO and type t = State.t = struct
  module IO = IO
  type 'a req_handler = In.Message.t Profuse.request -> 'a -> 'a IO.t
  type t = State.t

  let enosys req st =
    (* Throw away error messages during the write. *)
    let log_error _msg = () in
    IO.(Out.write_error log_error req Errno.ENOSYS >>= fun () -> return st)

  let getattr      = enosys
  let opendir _    = enosys
  let forget _     = enosys
  let lookup _     = enosys
  let readdir _    = enosys
  let readlink     = enosys
  let release _    = enosys
  let releasedir _ = enosys
  let open_ _      = enosys
  let read _       = enosys
  let access _     = enosys
  let destroy      = enosys

  let statfs       = enosys
  let getxattr _ _ = enosys
  let listxattr _  = enosys
  let interrupt _  = enosys
  let bmap _       = enosys

  let flush _      = enosys
  let link _ _     = enosys
  let symlink _ _  = enosys
  let rename _ _ _ = enosys
  let unlink _     = enosys
  let rmdir _      = enosys
  let mknod _ _    = enosys
  let write _ _    = enosys
  let mkdir _ _    = enosys
  let setattr _    = enosys

  let create _ _   = enosys
  let fsync _      = enosys
  let fsyncdir _   = enosys
  let setxattr _ _ = enosys
  let removexattr _= enosys
  let getlk _      = enosys
  let setlk _      = enosys
  let setlkw _     = enosys
end

module type MOUNT_IO = sig
  module IO : IO
  type t

  val mount :
    (Profuse.In.Init.T.t structure -> request -> t -> (request * t) IO.t) ->
    argv:string array -> mnt:string -> t -> (request * t) IO.t
end

module type MOUNT = functor (FS : FS_IO) ->
  MOUNT_IO with module IO = FS.IO
            and type t = FS.t

module type SERVER = sig
  module IO : IO
  type t

  val mount : argv:string array -> mnt:string -> t -> (request * t) IO.t

  val serve_forever : Profuse.chan -> t -> t IO.t
end

module Support(IO : IO) = struct
  open Profuse

  (* A typical mount negotiation *)
  let negotiate_mount pkt req st =
    let open Unsigned in
    let major = UInt32.to_int (Ctypes.getf pkt In.Init.T.major) in
    if major <> 7
    then IO.fail (
      ProtocolError
        (req.chan, Printf.sprintf
           "Incompatible FUSE protocol major version %d <> 7" major))
    else
      let minor = UInt32.to_int (Ctypes.getf pkt In.Init.T.minor) in
      if minor < 8
      then IO.fail (
        ProtocolError
          (req.chan, Printf.sprintf
             "Incompatible FUSE protocol minor version %d < 8" minor))
      else
        let minor = min minor 8 in (* TODO: track kernel minor *)
        let max_readahead = Ctypes.getf pkt In.Init.T.max_readahead in
        let max_readahead = max (UInt32.to_int max_readahead) 65536 in
        (* TODO: ? *)
        let _flags = Ctypes.getf pkt In.Init.T.flags in (* TODO: ? *)
        let chan = {
          req.chan with version = (major, minor); max_readahead; flags = 0l;
        } in
        let major = UInt32.of_int major in
        let minor = UInt32.of_int minor in
        let max_readahead = UInt32.of_int max_readahead in
        let max_write = UInt32.of_int req.chan.max_write in
        let pkt = Out.Init.create ~major ~minor ~max_readahead
            ~flags:UInt32.zero ~max_write in
        IO.(Out.write_reply req pkt
            >>= fun () ->
            return ({req with chan}, st)
           )

  let nodeid req = In.(Ctypes.getf req.hdr In.Hdr.T.nodeid)

  let enosys req st =
    (* Throw away error messages during the write. *)
    let log_error _msg = () in
    IO.(Out.write_error log_error req Errno.ENOSYS
        >>= fun () ->
        return st
       )

  let store_entry
    ?(entry_valid=Unsigned.UInt64.zero)
    ?(entry_valid_nsec=Unsigned.UInt32.zero)
    ?(attr_valid=Unsigned.UInt64.zero)
    ?(attr_valid_nsec=Unsigned.UInt32.zero)
    store_attr_of_node node = IO.(
    store_attr_of_node node (* can raise ENOENT *)
    >>= fun store_attr ->
    let nodeid = Unsigned.UInt64.of_int64 node.Nodes.id in
    let generation = Unsigned.UInt64.of_int64 node.Nodes.gen in
    return (Profuse.Out.Entry.store ~nodeid ~generation
              ~entry_valid ~entry_valid_nsec
              ~attr_valid ~attr_valid_nsec
              ~store_attr)
  )

  let respond_with_entry
    ?(entry_valid=Unsigned.UInt64.zero)
    ?(entry_valid_nsec=Unsigned.UInt32.zero)
    ?(attr_valid=Unsigned.UInt64.zero)
    ?(attr_valid_nsec=Unsigned.UInt32.zero)
    store_attr_of_node node req = IO.(
    store_attr_of_node node (* can raise ENOENT *)
    >>= fun store_attr ->
    let nodeid = Unsigned.UInt64.of_int64 node.Nodes.id in
    let generation = Unsigned.UInt64.of_int64 node.Nodes.gen in
    IO.Out.write_reply req
      (Profuse.Out.Entry.create ~nodeid ~generation
         ~entry_valid ~entry_valid_nsec
         ~attr_valid ~attr_valid_nsec
         ~store_attr)
  )
end

module Socket(IO : BASE_IO) = struct
  open Unsigned

  type t = {
    id    : int;
    read  : int -> uint8 Ctypes.CArray.t IO.t;
    write : uint8 Ctypes.ptr -> int -> int IO.t;
    nwrite: uint8 Ctypes.ptr -> int -> int IO.t;
    nread : unit -> uint32 IO.t;
  }

  let null = {
    id = -1;
    read  = (fun _ -> IO.return Ctypes.(CArray.make uint8_t 0));
    write = (fun _ len -> IO.return len);
    nwrite= (fun _ len -> IO.return len);
    nread = (fun _ -> IO.return UInt32.zero);
  }

  let table = ref (Array.make 0 null)

  (* TODO: release socket *)
  let new_ ~read ~write ~nwrite ~nread =
    let tab = !table in
    let next_id = Array.length tab in
    let tab = Array.init (next_id + 1) (fun i ->
      if i <> next_id
      then tab.(i)
      else { id = next_id; read; write; nwrite; nread }
    )  in
    table := tab;
    tab.(next_id)

  let id { id } = id

  let get k =
    (!table).(k)

  let read { read } = read

  let write { write } = write

  let nwrite { nwrite } = nwrite

  let nread { nread } = nread ()

  let set k ?read ?write ?nwrite ?nread () =
    let table = !table in
    if k >= Array.length table
    then raise (Invalid_argument "bad socket table index")
    else
      let socket = table.(k) in
      table.(k) <- {
        socket with
        read = (match read with
          | None -> socket.read
          | Some read -> read
        );
        write = (match write with
          | None -> socket.write
          | Some write -> write
        );
        nwrite = (match nwrite with
          | None -> socket.nwrite
          | Some nwrite -> nwrite
        );
        nread = (match nread with
          | None -> socket.nread
          | Some nread -> nread
        );
      }

  open Ctypes
  open Profuse

  let write_notify chan arr = IO.(
    let sz = CArray.length arr + Out.Hdr.sz in
    let ptr = CArray.start arr -@ Out.Hdr.sz in
    let socket = get chan.Profuse.id in
    nwrite socket (coerce (Ctypes.ptr char) (Ctypes.ptr uint8_t) ptr) sz
    >>= fun len ->
    if sz <> len
    then
      let msg =
        Printf.sprintf "Tried to write notify %d but only wrote %d" sz len
      in
      fail (Profuse.ProtocolError (chan,msg))
    else return ()
  )

  let read_notify chan = IO.(
    let socket = get chan.Profuse.id in
    nread socket
    >>= fun err ->
    if UInt32.(compare zero err) = 0
    then return []
    else
      let host = chan.host.Host.errno in
      let errno = Signed.SInt.of_int64 (UInt32.to_int64 err) in
      return (Errno.of_code ~host errno)
  )

  let write_reply_raw req sz ptr = IO.(
    let socket = get req.chan.id in
    write socket (coerce (Ctypes.ptr char) (Ctypes.ptr uint8_t) ptr) sz
    >>= fun len ->
    if sz <> len
    then
      let msg =
        Printf.sprintf "Tried to write %d but only wrote %d" sz len
      in
      fail (Profuse.ProtocolError (req.chan,msg))
    else return ()
  )
end

(* TODO: fix the Unix dependency due to Unix_error *)
module IO(IO : BASE_IO) : IO with type 'a t = 'a IO.t = struct
  open Ctypes
  open Unsigned
  open Profuse

  module Socket = Socket(IO)

  type 'a t = 'a IO.t

  let (>>=) = IO.(>>=)

  let return = IO.return

  let fail = IO.fail

  let catch = IO.catch

  module In = struct
    include In

    let remaining = ref None
    let parse chan n mem =
      let hdr_ptr = coerce (ptr uint8_t) (ptr Hdr.T.t) mem in
      let hdr = !@ hdr_ptr in
      chan.unique <- getf hdr Hdr.T.unique;
      let len = UInt32.to_int (getf hdr Hdr.T.len) in
      (if n < len
       then (* TODO: accumulate? *)
         let msg =
           Printf.sprintf "Packet has %d bytes but only read %d" len n
         in
         fail (ProtocolError (chan, msg))
       else if n > len
       then (remaining := Some (n - len, mem +@ len); return ())
       else return ()
      ) >>= fun () ->
      let len = len - Hdr.sz in
      let ptr = to_voidp (mem +@ Hdr.sz) in
      let message = Message.parse chan hdr len ptr in
      return message

    let read chan =
      let approx_page_size = 4096 in
      let count = chan.max_write + approx_page_size in
      fun () ->
        catch (fun () ->
          match !remaining with
          | None ->
            let socket = Socket.get chan.Profuse.id in
            Socket.read socket count
            >>= fun carray ->
            let ptr = Ctypes.CArray.start carray in
            let len = Ctypes.CArray.length carray in
            parse chan len ptr
          | Some (n, mem) ->
            remaining := None;
            parse chan n mem
        ) Unix.(function
          | Unix_error ((
            EINTR  (* SIGINT *)
          | ENODEV (* umount *)
          | EBADF  (* internal unmount *)
          ), "read", _) ->
            let nodeid = UInt64.zero in
            let uid = UInt32.zero in
            let gid = UInt32.zero in
            let pid = UInt32.zero in
            (* assumes sequentially increasing packet ids *)
            let unique = UInt64.succ chan.Profuse.unique in
            chan.Profuse.unique <- unique;
            let pkt = Hdr.packet ~opcode:`FUSE_DESTROY ~unique
                ~nodeid ~uid ~gid ~pid ~count:0
            in
            let hdr = !@ (coerce (ptr char) (ptr Hdr.T.t)
                            ((CArray.start pkt) -@ Hdr.sz)) in
            return Profuse.({ chan; hdr; pkt=Message.Destroy })
          | Unix_error (err, call, s) ->
            let msg =
              Printf.sprintf "%s(%s) error: %s" call s (error_message err)
            in
            fail (ProtocolError (chan, msg))
          | exn -> fail exn
        )

  end

  module Out = struct

    let write_reply req arrfn =
      let arr = arrfn req in
      let sz  = CArray.length arr + Out.Hdr.sz in
      let ptr = CArray.start arr -@ Out.Hdr.sz in
      Socket.write_reply_raw req sz ptr

    let write_ack req = write_reply req (Out.Hdr.packet ~count:0)

    let write_error log_error req err =
      let host = req.chan.host.Host.errno in
      let nerrno = match Errno.to_code ~host err with
        | Some errno -> Int64.to_int32 Signed.SInt.(to_int64 (neg errno))
        | None -> match Errno.to_code ~host Errno.EIO with
          | Some errno ->
            let errno_string = Errno.to_string err in
            log_error ("Couldn't find host error code for "^errno_string);
            Int64.to_int32 Signed.SInt.(to_int64 (neg errno))
          | None ->
            let errstr = Errno.to_string err in
            failwith (Printf.sprintf "errno for %s and EIO unknown" errstr)
      in
      write_reply req (Out.Hdr.packet ~nerrno ~count:0)
  end
end
