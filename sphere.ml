open Matrices
open Tuple
open Lights

let unique = 
  let last = ref 1 in 
  fun () -> incr last ; !last

class sphere =
  object(self)
   val _id = unique ()
   val mutable _transform = make_identity ()
   val mutable _material = default_material ()
  method id =
    _id
  method set_transform (m :float array array) =
    _transform <- m
  method transform =
    _transform
  method set_material (mat: material) =
    _material <- mat
  method material =
    _material
  method normal_at p : tuple =
    let trans_inverse = inverse _transform in
    let obj_point = matrix_tuple_mult trans_inverse p in
    let obj_normal = tuple_sub obj_point (point 0. 0. 0.) in
    let world_normal = matrix_tuple_mult (transpose trans_inverse) obj_normal in
    normalize {x=world_normal.x; y=world_normal.y; z=world_normal.z; w=0.}

end

(* let normal_at s p =
  normalize (tuple_sub p (point 0. 0. 0.)) *)
