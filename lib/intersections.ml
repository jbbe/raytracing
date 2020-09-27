open Matrices
open Rays
open Shapes
open Lights
open Tuple

type intersection = {t: float; obj: shape ref}
type computations = {
  t: float;
  obj: shape ref;
  point: tuple;
  eyev: tuple;
  normalv: tuple;
  reflectv: tuple;
  inside: bool;
  n1: float;
  n2: float;
  over_point: tuple;
  under_point: tuple;
}
type refraction_result = {n1:float; n2:float}

let _EPSILON = 0.0001

let null_x = {t=0.; obj=ref null_sphere}


let intersection_comp (a: intersection) (b:intersection) : int =
  compare a.t b.t 


let rec calc_n1_n2 ?(containers=[]) ?(n1=1.0) ?(n2=1.0) (hit: intersection) (xs: intersection list) : refraction_result =
  match xs with 
  | i::rest -> (let r_n1 : float = (if i = hit then match containers with 
      | _::_ ->  (!(List.nth containers (List.length containers - 1)))#material.refractive_idx
      | [] -> 1.0  else n1) in
    let c2 = if (List.mem i.obj containers) then List.filter (fun x -> x <> i.obj) containers else containers@[i.obj] in
    let r_n2: float = if i = hit 
    then match c2 with 
    | _::_ ->  (!(List.nth c2 (List.length c2 - 1)))#material.refractive_idx
    | [] -> 1.0 
    else n2 in
     calc_n1_n2 ~containers:c2 ~n1:r_n1 ~n2:r_n2 hit rest
  )
  | [] -> {n1=n1;n2=n2;}

let prepare_computations (i: intersection) (r: ray) (xs: intersection list): computations =
  let _point = (position r i.t) in
  let _pre_normalv = (!(i.obj)#normal_at _point) in
  let _eyev = (scalar_mult r.direction (-1.)) in
  let _inside = (dot _pre_normalv _eyev) < 0. in
  let _adjusted_normalv = if _inside then (scalar_mult _pre_normalv (-1.)) else _pre_normalv in
  let refract_vals = calc_n1_n2 i xs in
  let adjustment = scalar_mult _adjusted_normalv _EPSILON in
  {
    t=i.t;
    obj= i.obj;
    point=_point;
    eyev=_eyev;
    normalv=_adjusted_normalv;
    reflectv=(reflect r.direction _adjusted_normalv);
    inside=_inside;
    n1=refract_vals.n1;
    n2=refract_vals.n2;
    over_point= tuple_add _point adjustment;
    under_point= tuple_sub _point adjustment;
  }


let print_computations c =
  Printf.printf "t: %f inside: %b n1: %f n2: %f\n" c.t c.inside c.n1 c.n2;
  (!(c.obj))#print_shape;
  let t = c.point in
  Printf.printf "point : x = %f y = %f z = %f w = %f\n" t.x t.y t.z t.w;
  let op = c.over_point in
  Printf.printf "over_point : x = %f y = %f z = %f w = %f\n" op.x op.y op.z op.w;
  let up = c.under_point in
  Printf.printf "under_point : x = %f y = %f z = %f w = %f\n" up.x up.y up.z up.w;
  let nv = c.normalv in
  Printf.printf "normalv : x = %f y = %f z = %f w = %f\n" nv.x nv.y nv.z nv.w;
  let rv = c.reflectv in
  Printf.printf "reflectv : x = %f y = %f z = %f w = %f\n" rv.x rv.y rv.z rv.w


let local_intersect (s: shape) (r: ray) : intersection list  =
  match s#shape_type with 
    | Plane -> if ((Float.abs r.direction.y) < _EPSILON) then [] 
        else [{t=(((-1.) *. r.origin.y) /. r.direction.y); obj=ref s}]
    | Sphere -> 
        let sphere_to_ray = tuple_sub r.origin (point 0. 0. 0.) in
        let a = dot r.direction r.direction in
        let b = 2. *. (dot r.direction sphere_to_ray) in
        let c = (dot sphere_to_ray sphere_to_ray) -. 1. in
        let discriminant = (b *. b) -. (4. *. a *. c) in
        if discriminant < 0. then []
        else 
          let t1 = ((-1. *. b) -. (sqrt discriminant)) /. (2. *. a) in
          let t2 = ((-1. *. b) +. (sqrt discriminant)) /. (2. *. a) in
          [{t=t1; obj=ref s;}; {t=t2; obj=ref s}]
    
let intersect s r  =
  let local_ray = transform r (inverse s#transform) in
  local_intersect s local_ray

let rec _hit (xs:  intersection list) (lowest: intersection ) : intersection =
  match xs with
  | first::rest -> 
    if ((first.t >= 0.) && ((first.t < lowest.t) || lowest.t < 0.)) 
      then _hit rest first 
      else _hit rest lowest
  | [] -> if lowest.t >= 0. then lowest else null_x
  
let hit (xs: intersection list ) : intersection =
  match xs with 
  | first::rest -> _hit rest first
  | [] -> null_x
  
let rec intersections_on_list (shapes : shape list) (r: ray) : (intersection list)=
  match shapes with
  | s::rest -> List.append (intersect s r) (intersections_on_list rest r)
  | [] -> []

let print_intersection (x : intersection) =
  Printf.printf "t: %f obj" x.t;
  (!(x.obj))#print_shape 

let rec print_intersections xs =
  match xs with
    | first::rest -> print_intersection first; print_intersections rest
    | [] -> ()

let schlick comps : float=
  let cos_eye_normal = dot comps.eyev comps.normalv in
  let r_0 = ((comps.n1 -. comps.n2) /. (comps.n1 +. comps.n2)) ** 2. in
  if comps.n1 > comps.n2 
    then 
    let n = comps.n1 /. comps.n2 in
    let sin2_t : float = (n ** 2.) *. (1. -. (cos_eye_normal ** 2.)) in
    (if  sin2_t > 1. 
      then 1.
      else
        let cos_t = sqrt (1. -. sin2_t) in 
        r_0 +. ((1. -. r_0) *. ((1. -. cos_t) ** 5.))
    )
  else r_0 +. ((1. -. r_0) *. ((1. -. cos_eye_normal) ** 5.))