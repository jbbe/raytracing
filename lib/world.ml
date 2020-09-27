open Shapes
open Rays
open Lights
open Tuple
open Color
open Transformations
open Intersections

class world =
  object(self)
   val mutable _objects = ( [] : shape list )
   val mutable _lights = ( [] : light list )
  method lights =
    _lights
  method add_light l =
    _lights <- l::[]
  method objects : shape list =
    _objects
  method add_object o =
    _objects <- o::_objects
  method intersect (r : ray) : (intersection list) =
    List.sort intersection_comp (intersections_on_list _objects r) 
  method shade_hit ?(remaining=10) (comps : computations) =
    if remaining <= 0 then black else
    
    let _obj = (!(comps.obj)) 
    and _light = (List.hd _lights) 
    and _over_point = comps.over_point 
    and _eyev = comps.eyev 
    and _normalv = comps.normalv in
    let shadowed = self#is_shadowed comps.over_point in

    let surface = lighting _obj _light _over_point _eyev _normalv shadowed in
    let reflected = self#reflected_color ~remaining:(remaining - 1) comps  in
    let refracted = self#refracted_color ~remaining:(remaining - 1) comps in

    let _material = _obj#material in
    if _material.reflective > 0. && _material.transparency > 0. 
      then
        let reflectance = schlick comps in
        color_add 
          (color_add surface (color_scalar_mult reflected reflectance)) 
          (color_scalar_mult refracted (1. -. reflectance))
      else
        color_add (color_add surface reflected) refracted
  method color_at ?(remaining=10) (r : ray)  =
    if remaining <= 0 then black else
    let xs = self#intersect r in
    let _hit = hit xs in
    if _hit = null_x 
      then black 
      else let comps = prepare_computations _hit r xs in
      self#shade_hit ~remaining:(remaining - 1) comps 
  method is_shadowed (p: tuple) =
    let v = tuple_sub (List.hd _lights).position p in
    let distance = magnitude v in
    let r = {origin=p; direction=(normalize v)} in
    let xs = self#intersect r in
    (* Check if any hits are + shadow producing objects hit *)
    intersection_casts_shadow xs distance
    (* let h = hit xs in
    (h <> null_x && h.t < distance) *)
  method reflected_color ?(remaining=10) (comps : computations) =
    let reflectivity = (!(comps.obj))#material.reflective in
    if  (reflectivity <= _EPSILON || remaining <= 0) then  black else
    let reflect_ray = {origin=comps.over_point; direction=comps.reflectv} in
    let c = self#color_at ~remaining:(remaining - 1) reflect_ray  in
    color_scalar_mult c reflectivity
  method refracted_color ?(remaining=5) (comps : computations) =
    let transparency = (!(comps.obj))#material.transparency in
    (* Printf.printf "\ntransparency %f" transparency; *)
    if (transparency <= _EPSILON || remaining <= 0)  then black
    else
    (* inverse definition of snell's law *)
    let n_ratio = comps.n1 /. comps.n2 in
    let cos_i = dot comps.eyev comps.normalv in
    let sin2_t = (n_ratio ** 2. ) *. (1. -. (cos_i ** 2.)) in
    (* Printf.printf "\nSin2_t %f n_ration : %f cos_i : %f" sin2_t n_ratio cos_i; *)
    if sin2_t > 1. then black else
    let cos_t = sqrt (1. -. sin2_t) in
    let direction = tuple_sub (scalar_mult comps.normalv ((n_ratio *. cos_i) -. cos_t)) (scalar_mult comps.eyev  n_ratio) in
    let refract_ray = {origin=comps.under_point; direction=direction} in
    let unadjusted_color =  self#color_at ~remaining:(remaining-1) refract_ray in
    (* print_color unadjusted_color; *)
    color_scalar_mult unadjusted_color transparency 
  method print_world =
    Printf.printf "Print world, Objects:";
    List.iter (fun o -> o#print_shape) _objects;
    Printf.printf "World Light:\n";
    List.iter (fun l -> print_light l) _lights
end

let default_world (_ : unit) : world =
  let w = new world in
  let _light = point_light (point (-10.) 10. (-10.)) (white) in
  let s1 = new shape Sphere in 
  let m1 = {
            ambient=0.1; 
            diffuse=0.7;
            specular=0.2;
          shininess=200.;
          reflective=0.;
          transparency=0.;
          refractive_idx=1.;
          pattern= new pattern Solid [{r=0.8; g=1.; b=0.6}]} in
  s1#set_material m1;
  let s2 = new shape Sphere in
  s2#set_transform (scaling 0.5 0.5 0.5);
  w#add_object s2;
  w#add_object s1;
  w#add_light _light;
  w
