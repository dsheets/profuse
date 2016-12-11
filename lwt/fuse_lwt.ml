open Lwt
open Ctypes
open Unsigned
open Profuse

module type IO_LWT = Fuse.IO with type 'a t = 'a Lwt.t

module type FS_IO_LWT = Fuse.FS_IO with type 'a IO.t = 'a Lwt.t

module type FS_LWT = sig
  include Fuse.STATE

  val log_error : string -> unit

  module Calls :
    functor(IO : IO_LWT) ->
      FS_IO_LWT with type 'a IO.t = 'a IO.t and type t = t
end

module Socket = Fuse.Socket(Lwt)

module IO = Fuse.IO(Lwt)

(* TODO: Only depends on UNIX not LWT; we're conflating those deps for now *)
module Dispatch(F : FS_LWT) : FS_LWT with type t = F.t = struct
  type t = F.t

  let string_of_state = F.string_of_state
  let string_of_nodeid = F.string_of_nodeid

  let log_error = F.log_error

  module Calls(IO : IO_LWT) : FS_IO_LWT with type t = t = struct
    module Calls = F.Calls(IO)
    include Calls

    let dispatch req t =
      catch In.Message.(fun () -> match req.pkt with
        | Init _ -> fail (ProtocolError (req.chan, "INIT after mount"))
        | Getattr -> getattr req t
        | Opendir op -> opendir op req t
        | Forget f -> forget (Ctypes.getf f In.Forget.T.nlookup) req t
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

module type SERVER_LWT = Fuse.SERVER with type 'a IO.t = 'a Lwt.t

module type MOUNT_LWT =
  functor(F : FS_LWT)(IO : IO_LWT) ->
    Fuse.MOUNT_IO with type t = F.t and type 'a IO.t = 'a IO.t

module Server(M : MOUNT_LWT)(F : FS_LWT)(IO : IO_LWT)
  : SERVER_LWT with module IO = IO and type t = F.t =
struct
  module IO = IO
  module M = M(F)(IO)
  type t = F.t
  module Calls = F.Calls(IO)

  let mount = M.mount Calls.negotiate_mount

  let rec serve_forever chan t = IO.(
    IO.In.read chan ()
    >>= fun p ->
    async (fun () -> Calls.dispatch p t >>= fun _ -> return_unit);
    serve_forever chan t
  )
end
