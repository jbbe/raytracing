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
}

let prepare_computations (i: intersection) (r: ray) : computations =
  let p = (position r i.t) in
  let _normalv = (!(i.obj)#normal_at p) in
  let _eyev = (scalar_mult r.direction (-1.)) in
  let _inside = (dot _normalv _eyev) < 0. in
  {
    t=i.t;
    obj= i.obj;
    point=p;
    eyev=_eyev;
    normalv=if _inside then (scalar_mult _normalv (-1.)) else _normalv;
    inside=_inside
  }
  
