
module IO(IO : Fuse.BASE_IO)(Socket : Fuse.SOCKET with type 'a io = 'a IO.t)
: Fuse.IO with type 'a t = 'a IO.t

module Dispatch(F : Fuse.FS)
  : Fuse.FS with type t = F.t and module Calls.IO = F.Calls.IO
