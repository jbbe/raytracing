open Matrices
open Rays
open Tuple


class camera (hsize_in : int) (vsize_in : int) (fov_in : float) =
let tmp_aspect = (float_of_int hsize_in) /. (float_of_int vsize_in) in
let tmp_half_view = tan (fov_in /. 2.) in
let tmp_half_width =  if tmp_aspect >= 1. then tmp_half_view else tmp_half_view *. tmp_aspect in
object
    val _hsize = hsize_in
    val _vsize = vsize_in
    val _fov = fov_in
    val _half_view = tmp_half_view
    val _aspect = tmp_aspect
    val _half_width = tmp_half_width
    val _half_height = if tmp_half_view >= 1. then tmp_half_view *. tmp_half_view else tmp_half_view
    val _pixel_size = (tmp_half_width *. 2.) /. (float_of_int hsize_in)
    val mutable _transform = make_identity ()
  method set_transform mat =
    _transform <- mat
  method transform =
    _transform
  method hsize =
    _hsize
  method vsize =
    _vsize
  method fov =
    _fov
  method pixel_size =
    _pixel_size
  method ray_for_pixel (px: int) (py: int) =
    (* Offset from edge of canvas to pixel's center *)
    let xoffset = ((float_of_int px) +. 0.5) *. _pixel_size in
    let yoffset = ((float_of_int py) +. 0.5) *. _pixel_size in
    (* Untransformed coords of the pixel in world space *)
    (* Camera looks toward -z so +x is to the LEFT *)
    let world_x = _half_width -. xoffset in
    let world_y = _half_height -. yoffset in
    (* use camera matrix to transform canvas point and origin *)
    (* Then compute ray's direction vec *)
    (* Canvas is at z=-1 *)
    let pixel = matrix_tuple_mult (inverse _transform) (point world_x world_y (-1.))in
    let origin = matrix_tuple_mult (inverse _transform) (point 0. 0. 0.) in
    let direction = normalize (tuple_sub pixel origin) in
    {origin=origin; direction=direction}

end