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
