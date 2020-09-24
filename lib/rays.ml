open Tuple
open Matrices
open Shapes 

type ray = {origin: tuple; direction: tuple}

let null_sphere = new shape Sphere

let position (r: ray) (t: float) =
  tuple_add r.origin (scalar_mult r.direction t)

let transform (r : ray) (m: float array array) : ray =
  let new_origin = matrix_tuple_mult m r.origin in
  let new_direction =  matrix_tuple_mult m r.direction in
  {origin=new_origin; direction=new_direction}

let print_ray r =
  let o = r.origin
  and d = r.direction in
  Printf.printf "Ray, origin: x=%f y=%f z=%f w=%f dir: x=%f y=%f z=%f w=%f\n" o.x o.y o.z o.w d.x d.y d.z d.w
