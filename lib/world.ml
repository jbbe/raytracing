open Sphere
open Rays
open Lights
open Tuple
open Color
open Transformations

class world =
  object(self)
   val mutable _objects = ( [] : sphere list )
   val mutable _lights = ( [] : light list )
  method lights =
    _lights
  method add_light l =
    _lights <- l::_lights
  method objects : sphere list =
    _objects
  method add_object o =
    _objects <- o::_objects
  method intersect (r : ray) : (intersection list) =
    List.sort intersection_comp (intersections_on_list _objects r) 
  method print =
    Printf.printf "World light: "; print_spheres (self#objects);
end

let default_world (_ : unit) : world =
  let w = new world in
  let _light = point_light (point (-10.) 10. (-10.)) (white ()) in
  let s1 = new sphere in 
  let m1 = {color={r=0.8; g=1.; b=0.6};
            ambient=0.1; 
            diffuse=0.7;
            specular=0.2;
          shininess=200.} in
  s1#set_material m1;
  let s2 = new sphere in
  s2#set_transform (scaling 0.5 0.5 0.5);
  w#add_object s1;
  w#add_object s2;
  w#add_light _light;
  w
