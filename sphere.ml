open Matrices

let unique = 
  let last = ref 1 in 
  fun () -> incr last ; !last

class sphere =
  object(self)
   val _id = unique ()
   val mutable _transform = make_identity ()
  method id =
    _id
  method set_transform (m :float array array) =
    _transform <- m
  method transform =
    _transform
end