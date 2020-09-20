open Matrices
open Rays
open Shapes
open Tuple

type intersection = {t: float; obj: shape ref}
type computations = {
  t: float;
  obj: shape ref;
  point: tuple;
  eyev: tuple;
  normalv: tuple;
  inside: bool;
  over_point: tuple;
}


let _EPSILON = 0.0001
let null_x = {t=0.; obj=ref null_sphere}


let intersection_comp (a: intersection) (b:intersection) : int =
  compare a.t b.t 


let prepare_computations (i: intersection) (r: ray) : computations =
  let _point = (position r i.t) in
  let _pre_normalv = (!(i.obj)#normal_at _point) in
  let _eyev = (scalar_mult r.direction (-1.)) in
  let _inside = (dot _pre_normalv _eyev) < 0. in
  let _normalv = if _inside then (scalar_mult _pre_normalv (-1.)) else _pre_normalv in
  {
    t=i.t;
    obj= i.obj;
    point=_point;
    eyev=_eyev;
    normalv=_normalv;
    inside=_inside;
    over_point= tuple_add _point (scalar_mult _normalv _EPSILON);
  }

  let intersect (s: shape) (r: ray) : intersection list  =
    match s#shape_type with 
    | Plane -> if ((Float.abs r.direction.y) < _EPSILON) then [] else []
    | Sphere ->
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
      [{t=t1; obj=ref s;}; {t=t2; obj=ref s}]
  
  let rec _hit (xs:  intersection list) (lowest: intersection ) : intersection =
    match xs with
    | first::rest -> 
      if ((first.t >= 0.) && ((first.t < lowest.t) || lowest.t < 0.)) then _hit rest first else _hit rest lowest
    | [] -> if lowest.t >= 0. then lowest else null_x
  
  let hit (xs: intersection list ) : intersection =
    match xs with 
    | first::rest -> _hit rest first
    | [] -> null_x
  
  let rec intersections_on_list (shapes : shape list) (r: ray) : (intersection list)=
    match shapes with
    | s::rest -> List.append (intersect s r) (intersections_on_list rest r)
    | [] -> []

(* let rec print_intersections xs =
  match xs with
    | first::rest -> Printf.printf "t: %f obj %d\n" first.t (!(first.obj))#id; print_intersections rest
    | [] -> () *)