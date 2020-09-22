open OUnit2
open Raytracing.Tuple
open Raytracing.Color
open Raytracing.Shapes
open Raytracing.Lights
open Raytracing.Transformations

let w = white
let b = black

let test_point_light_basic _ =
  let i = {r=1.; g=1.; b=1.} in
  let p = point 0. 0. 0. in
  let l = point_light p i in
  assert_equal l.position p;
  assert_equal l.intensity i 

let test_default_material _ =
  let m =  default_material () in
  assert_bool "equal pats" (pattern_equal m.pattern (new pattern Solid [{r=1.; g=1.; b=1.}]));
  assert_equal m.ambient 0.1;
  assert_equal m.diffuse 0.9;
  assert_equal m.specular 0.9;
  assert_equal m.shininess 200.
  
let test_sphere_material _ =
  let s = new shape Sphere in
  let m = s#material in
  assert_bool "sphere mat" (material_equal m (default_material ()))

let test_sphere_mat_assignment _ =
  let s = new shape Sphere  in
  let m = default_material () in
  m.ambient <- 1.;
  s#set_material m;
  assert_bool "sphere mat ass" (material_equal (s#material) m)

let test_level_lighting _ =
  let m = default_material () in
  let pos = point 0. 0. 0. in
  let eyev = vector 0. 0. (-1.) in
  let normalv = vector 0. 0. (-1.) in
  let light = point_light (point 0. 0. (-10.)) (white ) in
  let result = lighting m light pos eyev normalv false in
  (* print_color result; *)
  assert_bool "level lighting" (color_equal {r=1.9; g=1.9; b=1.9} result)

let test_lighting_offset_45deg _ =
  let deg45 = (sqrt 2.) /. 2. in
  let m = default_material () in
  let pos = point 0. 0. 0. in
  let eyev = vector 0. deg45 ((-1.) *. deg45) in
  let normalv = vector 0. 0. (-1.) in
  let light = point_light (point 0. 0. (-10.)) (white ) in
  let result = lighting m light pos eyev normalv false in
  assert_equal {r=1.; g=1.; b=1.} result  

let test_lighting_eye_opposite_light_offset_45deg _ =
  let m = default_material () in
  let pos = point 0. 0. 0. in
  let eyev = vector 0. 0. (-1.) in
  let normalv = vector 0. 0. (-1.) in
  let light = point_light (point 0. 10. (-10.)) (white ) in
  let result = lighting m light pos eyev normalv false in
  assert_bool "lighting eye oppo" (color_equal {r=0.7364; g=0.7364; b=0.7364} result)

let test_lighting_eye_in_path_of_reflection _ =
  let negdeg45 = (-1.) *. (sqrt 2.) /. 2. in
  let m = default_material () in
  let pos = point 0. 0. 0. in
  let eyev = vector 0. negdeg45 negdeg45 in
  let normalv = vector 0. 0. (-1.) in
  let light = point_light (point 0. 10. (-10.)) (white) in
  let result = lighting m light pos eyev normalv false in
  assert_bool "eye in path" (color_equal {r=1.6364; g=1.6364; b=1.6364} result)

let test_light_behind_surface _ =
  let m = default_material () in
  let pos = point 0. 0. 0. in
  let eyev = vector 0. 0. (-1.) in
  let normalv = vector 0. 0. (-1.) in
  let light = point_light (point 0. 0. (10.)) (white) in
  let result = lighting m light pos eyev normalv false in
  assert_equal {r=0.1; g=0.1; b=0.1} result

let test_light_in_shadow _ =
  let m = default_material () in
  let pos = point 0. 0. 0. in
  let eyev = vector 0. 0. (-1.) in
  let normalv = vector 0. 0. (-1.) in
  let light = point_light (point 0. 0. (-10.)) (white) in
  let result = lighting m light pos eyev normalv true in
  assert_equal {r=0.1; g=0.1; b=0.1} result  

let test_stripe_pattern _ =
  let _p = stripe_pattern (white) (black) in
  assert_equal _p#first_color w;
  assert_equal _p#second_color b

let test_stripe_pattern_constant_in_y _ =
  let _p = stripe_pattern (white) (black) in
  assert_equal w (_p#color_at (point 0. 0. 0.));
  assert_equal w (_p#color_at (point 0. 1. 0.));
  assert_equal w (_p#color_at (point 0. 2. 0.))

let test_stripe_pattern_constant_in_z _ =
  let p = stripe_pattern (white) (black) in
  assert_equal w (p#color_at (point 0. 0. 1.));
  assert_equal w (p#color_at (point 0. 2. 2.))


let test_stripe_pattern_alternates_in_x _ =
  let p = stripe_pattern (white) (black) in
  assert_equal w (p#color_at (point 0. 0. 0.));
  assert_equal w (p#color_at (point 0.9 0. 0.));
  assert_equal b (p#color_at (point 1. 0. 0.));
  assert_equal b (p#color_at (point 1. 0. 0.));
  assert_equal b (p#color_at (point (-0.1) 0. 0.));
  assert_equal b (p#color_at (point (-1.) 0. 0.));
  assert_equal w (p#color_at (point (-1.1) 0. 0.))

let test_light_pattern _ =
  let m =  { 
    ambient=1.; 
    diffuse=0.;
    specular=0.;
    shininess=200.;
    pattern=(new pattern Stripe [white; black]);
    } in

  let eyev = vector 0. 0. (-1.) in
  let normalv = vector 0. 0. (-1.) in
  let _light = point_light (point 0. 0. (-10.)) w in
  let c1 = lighting m _light (point 0.9 0. 0.) eyev normalv false in
  let c2 = lighting m _light (point 1.1 0. 0.) eyev normalv false in
  (* print_color c1;
  print_color c2; *)
  assert_equal c1 w;
  assert_equal c2 b

let test_stripes_with_obj_trans _ =
  let obj = new shape Sphere in
  obj#set_transform (scaling 2. 2. 2.);
  let pat = stripe_pattern white black in
  obj#set_pattern pat;
  let c = obj#color_at (point 1.5 0. 0.) in
  assert_equal c white

let test_stripe_with_pat_trans _ =
  let obj = new shape Sphere in
  let pat = stripe_pattern w b in
  pat#set_transform (scaling 2. 2. 2.);
  obj#set_pattern pat;
  let c = obj#color_at (point 1.5 0. 0.) in
  (* print_color c; *)
  assert_equal c w

let test_double_trans_pat_obj _ =
  let obj = new shape Sphere in
  let pat = stripe_pattern w b in
  pat#set_transform (scaling 2. 2. 2.);
  obj#set_pattern pat;
  obj#set_transform (scaling 2. 2. 2.);
  let c = obj#color_at (point 1.5 0. 0.) in
  assert_equal c w


let suite =
  "LightsList" >::: [
    "test_point_light_basic" >:: test_point_light_basic;
    "test_default_material" >:: test_default_material;
    "test_sphere_material" >:: test_sphere_material;
    "test_sphere_mat_assignment" >:: test_sphere_mat_assignment;
    "test_level_lighting" >:: test_level_lighting;
    "test_lighting_offset_45deg" >:: test_lighting_offset_45deg;
    "test_lighting_eye_opposite_light_offset_45deg" >:: test_lighting_eye_opposite_light_offset_45deg;
    "test_lighting_eye_in_path_of_reflection" >:: test_lighting_eye_in_path_of_reflection;
    "test_light_behind_surface" >:: test_light_behind_surface;
    "test_light_in_shadow" >:: test_light_in_shadow;
    "test_stripe_pattern" >:: test_stripe_pattern;
    "test_stripe_pattern_constant_in_y" >:: test_stripe_pattern_constant_in_y;
    "test_stripe_pattern_constant_in_z" >:: test_stripe_pattern_constant_in_z;
    "test_stripe_pattern_alternates_in_x" >:: test_stripe_pattern_alternates_in_x;
    "test_stripe_pattern_alternates_in_x" >:: test_stripe_pattern_alternates_in_x;
    "test_light_pattern" >:: test_light_pattern;
    "test_stripes_with_obj_trans" >:: test_stripes_with_obj_trans;
    "test_stripe_with_pat_trans" >:: test_stripe_with_pat_trans;
    "test_double_trans_pat_obj" >:: test_double_trans_pat_obj;
  ]

let () =
  run_test_tt_main suite