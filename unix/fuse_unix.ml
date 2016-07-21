
open Fuse

(* TODO: fix the Unix dependency due to Unix_error *)
module IO(IO : BASE_IO)(Socket : SOCKET with type 'a io = 'a IO.t)
  : IO with type 'a t = 'a IO.t = struct
  open Ctypes
  open Unsigned
  open Profuse

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
            let socket = Socket.get_int chan.Profuse.id in
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

    let read_notify chan () = Socket.read_notify chan

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

    let write_notify = Socket.write_notify
  end
end

(* TODO: fix dependency on Unix for Unix_error *)
module Dispatch(F : FS)
  : FS with type t = F.t and module Calls.IO = F.Calls.IO = struct
  type t = F.t

  let string_of_state = F.string_of_state
  let string_of_nodeid = F.string_of_nodeid

  let log_error = F.log_error

  module Calls : FS_IO with type t = t and module IO = F.Calls.IO = struct
    include F.Calls

    open Profuse

    let dispatch req t =
      IO.catch In.Message.(fun () -> match req.pkt with
        | Init _ -> IO.fail (ProtocolError (req.chan, "INIT after mount"))
        | Getattr -> getattr req t
        | Opendir op -> opendir op req t
        | Forget f -> forget (Ctypes.getf f In.Forget.T.nlookup) req t
        | Batch_forget b -> batch_forget b req t
        | Lookup name -> lookup name req t
        | Readdir r -> readdir r req t
        | Readlink -> readlink req t
        | Releasedir r -> releasedir r req t
        | Open op -> open_ op req t
        | Read r -> read r req t
        | Flush f -> flush f req t
        | Release r -> release r req t
        | Symlink (name,target) -> symlink name target req t
        | Rename (r,src,dest) -> rename r src dest req t
        | Unlink name -> unlink name req t
        | Rmdir name -> rmdir name req t
        | Statfs -> statfs req t
        | Fsync f -> fsync f req t
        | Write (w, data) -> write w data req t
        | Link (l,name) -> link l name req t
        | Getxattr (g,name) -> getxattr g name req t
        | Setxattr (s,name) -> setxattr s name req t
        | Listxattr g -> listxattr g req t
        | Removexattr name -> removexattr name req t
        | Access a -> access a req t
        | Create (c,name) -> create c name req t
        | Mknod (m,name) -> mknod m name req t
        | Mkdir (m,name) -> mkdir m name req t
        | Fsyncdir f -> fsyncdir f req t
        | Getlk lk  -> getlk  lk req t
        | Setlk lk  -> setlk  lk req t
        | Setlkw lk -> setlkw lk req t
        | Interrupt i -> interrupt i req t
        | Bmap b -> bmap b req t
        | Destroy -> destroy req t
        | Setattr s -> setattr s req t
        | Other _ | Unknown _ ->
          IO.(Out.write_error log_error req Errno.ENOSYS
              >>= fun () -> return t)
      ) (function
        | Unix.Unix_error(e, _, _) as exn ->
          let host = req.chan.host.Host.errno in
          let errno = match Errno_unix.of_unix ~host e with
            | [] ->
              let error_string = Printexc.to_string exn in
              log_error ("Couldn't find host errno for "^error_string);
              Errno.EIO
            | errno::_ -> errno
          in
          IO.(Out.write_error log_error req errno
              >>= fun () ->
              return t
             )
        | Errno.Error { Errno.errno = errno :: _ } ->
          IO.(Out.write_error log_error req errno
              >>= fun () ->
              return t
             )
        | (Destroy k) as exn -> IO.fail exn
        | exn ->
          log_error ("Unknown exception caught: "^(Printexc.to_string exn));
          IO.(Out.write_error log_error req Errno.EIO
              >>= fun () -> fail exn)
      )
  end
end
