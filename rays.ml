open Tuple
open Matrices
open Sphere


type ray = {origin: tuple; direction: tuple}
type intersection = {t: float; obj: int}
let null_x = {t=0.; obj=0}

let rec print_intersections xs =
  match xs with
    | first::rest -> Printf.printf "t: %f obj %d\n" first.t first.obj; print_intersections rest
    | [] -> ()

let position (r: ray) (t: float) =
  tuple_add r.origin (scalar_mult r.direction t)

let transform (r : ray) (m: float array array) : ray =
  let new_origin = matrix_tuple_mult m r.origin in
  let new_direction =  matrix_tuple_mult m r.direction in
  {origin=new_origin; direction=new_direction}

let intersect (s: sphere) (r: ray) : intersection list  =
  let r2 = transform r (inverse s#transform) in
  let sphere_to_ray = tuple_sub r2.origin (point 0. 0. 0.) in
  let a = dot r2.direction r2.direction in
  let b = 2. *. (dot r2.direction sphere_to_ray) in
  let c = (dot sphere_to_ray sphere_to_ray) -. 1. in
  let discriminant = (b *. b) -. (4. *. a *. c) in
  if discriminant < 0. then []
  else 
    let t1 = ((-1. *. b) -. (sqrt discriminant)) /. (2. *. a) in
    let t2 = ((-1. *. b) +. (sqrt discriminant)) /. (2. *. a) in
    [{t=t1; obj=s#id;}; {t=t2; obj=s#id}]


let rec _hit xs lowest =
  match xs with
  | first::rest -> 
    if ((first.t >= 0.) && ((first.t < lowest.t) || lowest.t < 0.)) then _hit rest first else _hit rest lowest
  | [] -> if lowest.t >= 0. then lowest else null_x

let hit xs =
  match xs with 
  | first::rest -> _hit rest first
  | [] -> null_x

