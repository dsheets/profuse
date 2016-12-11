open Lwt
open Ctypes
open Unsigned
open Profuse

module type IO_LWT = Fuse.IO with type 'a t = 'a Lwt.t

module type FS_IO_LWT = Fuse.FS_IO with type 'a IO.t = 'a Lwt.t

module IO = Fuse.IO(Lwt)

module type FS_LWT = Fuse.FS with module Calls.IO = IO

module Socket = Fuse.Socket(Lwt)

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
  module Calls = F.Calls

  let mount = M.mount Calls.negotiate_mount

  let rec serve_forever chan t = IO.(
    IO.In.read chan ()
    >>= fun p ->
    async (fun () -> Calls.dispatch p t >>= fun _ -> return_unit);
    serve_forever chan t
  )
end
