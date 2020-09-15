open Tuple
open Sphere


type ray = {origin: tuple; direction: tuple}
type intersection = {t: float; obj: int}

let rec print_intersections xs =
  match xs with
    | first::rest -> Printf.printf "t: %f obj %d\n" first.t first.obj; print_intersections rest
    | [] -> ()

let position (r: ray) (t: float) =
  tuple_add r.origin (scalar_mult r.direction t)

let intersect (s: sphere) (r: ray) : intersection list  =
  let sphere_to_ray = tuple_sub r.origin (point 0. 0. 0.) in
  let a = dot r.direction r.direction in
  let b = 2. *. (dot r.direction sphere_to_ray) in
  let c = (dot sphere_to_ray sphere_to_ray) -. 1. in
  let discriminant = (b *. b) -. (4. *. a *. c) in
  if discriminant < 0. then []
  else 
    let t1 = ((-1. *. b) -. (sqrt discriminant)) /. (2. *. a) in
    let t2 = ((-1. *. b) +. (sqrt discriminant)) /. (2. *. a) in
    [{t=t1; obj=s#id;}; {t=t2; obj=s#id}]
