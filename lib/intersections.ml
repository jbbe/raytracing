open Tuple
open Rays
open Sphere

type computations = {
  t: float;
  obj: sphere ref;
  point: tuple;
  eyev: tuple;
  normalv: tuple;
  inside: bool;
  over_point: tuple;
}

let _EPSILON = 0.0001

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

