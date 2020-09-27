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
    val mutable _casts_shadow = true;
    val mutable _transform = make_identity ()
    val mutable _material = default_material ()
    method id = _id
    method shape_type = _shape_type
    method set_transform (m :float array array) = _transform <- m
    method transform = _transform
    method set_material (mat: material) = _material <- mat
    method set_pattern (pat: pattern) = _material.pattern <- pat
    method material = _material
    method casts_shadow = _casts_shadow
    method set_casts_shadow cast_in = _casts_shadow <- cast_in
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
      (* Printf.printf "Shape#color at, wolrd_point:";
      print_tuple world_point; *)
      let object_point = matrix_tuple_mult (inverse _transform) world_point in
      let pattern_point = matrix_tuple_mult (inverse _material.pattern#transform) object_point in
      _material.pattern#color_at pattern_point
    method print_shape = 
      Printf.printf "\nShape %s id: %d" (if _shape_type = Sphere then "sphere" else "plane") _id; 
      print_matrix _transform;
      print_material _material
end

let glass_sphere () =
  let g = new shape Sphere in
  let m = default_material () in
  m.transparency <- 1.0;
  m.refractive_idx <- 1.5;
  g#set_material m;
  g

(* let print_shape_refs objs   *)
let lighting (_object : shape) _light _point _eyev _normalv (in_shadow : bool) =
  (* combine surface color with the light's color/intensity *)
  let _color = _object#color_at _point in
  let _material = _object#material in
  let effective_color = schur_prod _color _light.intensity in

  (* find the direction of light source *)
  let lightv = normalize (tuple_sub _light.position _point) in

  (* compute ambient contribution *)
  let ambient = color_scalar_mult effective_color _material.ambient in
  if in_shadow then ambient else 
  (* light_dot_normal is cosine of angle between light vec and normal vec
  negative value means the light is on the other side of the surface *)
  let light_dot_normal = dot lightv _normalv in

  if light_dot_normal < 0. 
    then color_add (color_add (black) (black)) ambient
    else 
      let diffuse = color_scalar_mult (color_scalar_mult effective_color _material.diffuse) light_dot_normal in
      (* reflect_dot_eye represents the cosine of the angle between the​
        reflection vector and the eye vector. A negative number means the​
        light reflects away from the eye.​ *)
      let reflectv = reflect (scalar_mult lightv (-1.)) _normalv in
      let reflect_dot_eye = dot reflectv _eyev in
      let specular = (if reflect_dot_eye <= 0. 
        then black 
        else color_scalar_mult (color_scalar_mult _light.intensity _material.specular) (reflect_dot_eye ** _material.shininess)
      ) in
      color_add (color_add ambient diffuse) specular
  