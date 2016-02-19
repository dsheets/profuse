
module type IO_LWT = Fuse.IO with type 'a t = 'a Lwt.t

module type FS_IO_LWT = Fuse.FS_IO with type 'a IO.t = 'a Lwt.t

module type FS_LWT = sig
  include Fuse.STATE

  module Calls :
    functor(IO : IO_LWT) ->
      FS_IO_LWT with type 'a IO.t = 'a IO.t and type t = t
end

module IO : IO_LWT

module Trace : functor (F : FS_LWT) -> FS_LWT with type t = F.t

module Dispatch : functor (F : FS_LWT) -> FS_LWT with type t = F.t

module type SERVER_LWT = Fuse.SERVER with type 'a IO.t = 'a Lwt.t

module type MOUNT_LWT =
  functor(F : FS_LWT)(IO : IO_LWT) ->
    Fuse.MOUNT_IO with type t = F.t and type 'a IO.t = 'a IO.t

module Server(M : MOUNT_LWT)(F : FS_LWT)(IO : IO_LWT)
  : SERVER_LWT with module IO = IO and type t = F.t

type socket

val new_socket :
  read:(int -> Unsigned.uint8 Ctypes.CArray.t Lwt.t) ->
  write:(Unsigned.uint8 Ctypes.ptr -> int -> int Lwt.t) ->
  socket

val socket_id : socket -> int

val get_socket : int -> socket

val set_socket :
  int ->
  ?read:(int -> Unsigned.uint8 Ctypes.CArray.t Lwt.t) ->
  ?write:(Unsigned.uint8 Ctypes.ptr -> int -> int Lwt.t) ->
  unit -> unit
