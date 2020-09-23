open Color
open Matrices
open Lights
open Tuple

type shape_type = Sphere | Plane

let unique = 
  let last = ref 1 in 
  fun () -> incr last ; !last


class shape shape_type_in =
  object 
    val _id = unique ()
    val _shape_type : shape_type = shape_type_in
    val mutable _transform = make_identity ()
    val mutable _material = default_material ()
    method id = _id
    method shape_type = _shape_type
    method set_transform (m :float array array) = _transform <- m
    method transform = _transform
    method set_material (mat: material) = _material <- mat
    method set_pattern (pat: pattern) = _material.pattern <- pat
    method material = _material
    method normal_at (_point : tuple) : tuple =
      match _shape_type with
      | Sphere -> 
        let trans_inverse = inverse _transform in
        let obj_point = matrix_tuple_mult trans_inverse _point in
        let obj_normal = tuple_sub obj_point (point 0. 0. 0.) in
        let world_normal = matrix_tuple_mult (transpose trans_inverse) obj_normal in
        normalize {x=world_normal.x; y=world_normal.y; z=world_normal.z; w=0.}
      | Plane -> vector 0. 1. 0.
   method color_at (world_point: tuple) : color =
      let object_point = matrix_tuple_mult (inverse _transform) world_point in
      let pattern_point = matrix_tuple_mult (inverse _material.pattern#transform) object_point in
      _material.pattern#color_at pattern_point
end

