
module IO(IO : Fuse.BASE_IO) : Fuse.IO with type 'a t = 'a IO.t

module Dispatch(F : Fuse.FS) : Fuse.FS with type t = F.t
