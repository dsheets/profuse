
module type IO_LWT = Fuse.IO with type 'a t = 'a Lwt.t

module type FS_IO_LWT = Fuse.FS_IO with type 'a IO.t = 'a Lwt.t

module IO : IO_LWT

module type FS_LWT = Fuse.FS with module IO = IO

module Dispatch : functor (F : FS_LWT) -> FS_LWT with type t = F.t

module type SERVER_LWT = Fuse.SERVER with type 'a IO.t = 'a Lwt.t

module type MOUNT_LWT =
  functor(F : FS_LWT)(IO : IO_LWT) ->
    Fuse.MOUNT_IO with type t = F.t and type 'a IO.t = 'a IO.t

module Server(M : MOUNT_LWT)(F : FS_LWT)(IO : IO_LWT)
  : SERVER_LWT with module IO = IO and type t = F.t
