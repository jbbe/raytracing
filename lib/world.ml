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
    (* Printf.printf "Shade hit enter\n"; *)

    let shadowed = self#is_shadowed comps.over_point in


    let surface = lighting _obj _light _over_point _eyev _normalv shadowed in
    let reflected = self#reflected_color ~remaining:(remaining - 1) comps  in

    (* Printf.printf "Shade hit return surface then reflected:\n"; 
    print_color surface;
    print_color reflected; *)

    color_add surface reflected
  method color_at ?(remaining=10) (r : ray)  =
    if remaining <= 0 then black else
    let xs = self#intersect r in
    let _hit = hit xs in
    (* Printf.printf "world#Color at enter\n";
    print_intersections xs;
    Printf.printf "\n_hit vvvvvv \n"; 
 
    print_intersection _hit; *)
    (* Printf.printf "\n ^^^^^^^^^^^ \n\n\n";  *)
    if _hit = null_x 
      then black 
      else let comps = prepare_computations _hit r in
      (* Printf.printf "world#Color at computations:  "; *)
      (* print_computations comps; *)
      self#shade_hit ~remaining:(remaining - 1) comps 
  method is_shadowed (p: tuple) =
    let v = tuple_sub (List.hd _lights).position p in
    let distance = magnitude v in
    let r = {origin=p; direction=(normalize v)} in
    let xs = self#intersect r in
    let h = hit xs in
    (h <> null_x && h.t < distance)
  method reflected_color ?(remaining=10) (comps : computations) =
    let reflectivity = (!(comps.obj))#material.reflective in
    (* if  (reflectivity <= _EPSILON || remaining <= 0) then (Printf.printf "\nreflected color returning black"; black) else *)
    if  (reflectivity <= _EPSILON || remaining <= 0) then  black else
    let reflect_ray = {origin=comps.over_point; direction=comps.reflectv} in
    let c = self#color_at ~remaining:(remaining - 1) reflect_ray  in
    (* Printf.printf "\nReflected_color color_at returns : \n"; *)
    (* print_color c; *)
    color_scalar_mult c reflectivity
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
          pattern= new pattern Solid [{r=0.8; g=1.; b=0.6}]} in
  s1#set_material m1;
  let s2 = new shape Sphere in
  s2#set_transform (scaling 0.5 0.5 0.5);
  w#add_object s2;
  w#add_object s1;
  w#add_light _light;
  w
