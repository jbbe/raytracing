open OUnit2
open Raytracing.Tuple
open Raytracing.Color
open Raytracing.Sphere
open Raytracing.Lights

let test_point_light_basic _ =
  let i = {r=1.; g=1.; b=1.} in
  let p = point 0. 0. 0. in
  let l = point_light p i in
  assert_equal l.position p;
  assert_equal l.intensity i 

let test_default_material _ =
  let m =  default_material () in
  assert_equal m.color {r=1.; g=1.; b=1.};
  assert_equal m.ambient 0.1;
  assert_equal m.diffuse 0.9;
  assert_equal m.specular 0.9;
  assert_equal m.shininess 200.
  
let test_sphere_material _ =
  let s = new sphere in
  let m = s#material in
  assert_equal m (default_material ())

let test_sphere_mat_assignment _ =
  let s = new sphere in
  let m = default_material () in
  m.ambient <- 1.;
  s#set_material m;
  assert_equal (s#material) m

let test_level_lighting _ =
  let m = default_material () in
  let pos = point 0. 0. 0. in
  let eyev = vector 0. 0. (-1.) in
  let normalv = vector 0. 0. (-1.) in
  let light = point_light (point 0. 0. (-10.)) (white ()) in
  let result = lighting m light pos eyev normalv false in
  assert_equal {r=1.9; g=1.9; b=1.9} result

let test_lighting_offset_45deg _ =
  let deg45 = (sqrt 2.) /. 2. in
  let m = default_material () in
  let pos = point 0. 0. 0. in
  let eyev = vector 0. deg45 ((-1.) *. deg45) in
  let normalv = vector 0. 0. (-1.) in
  let light = point_light (point 0. 0. (-10.)) (white ()) in
  let result = lighting m light pos eyev normalv false in
  assert_equal {r=1.; g=1.; b=1.} result  

let test_lighting_eye_opposite_light_offset_45deg _ =
  let m = default_material () in
  let pos = point 0. 0. 0. in
  let eyev = vector 0. 0. (-1.) in
  let normalv = vector 0. 0. (-1.) in
  let light = point_light (point 0. 10. (-10.)) (white ()) in
  let result = lighting m light pos eyev normalv false in
  assert_bool "lighting eye oppo" (color_equal {r=0.7364; g=0.7364; b=0.7364} result)

let test_lighting_eye_in_path_of_reflection _ =
  let negdeg45 = (-1.) *. (sqrt 2.) /. 2. in
  let m = default_material () in
  let pos = point 0. 0. 0. in
  let eyev = vector 0. negdeg45 negdeg45 in
  let normalv = vector 0. 0. (-1.) in
  let light = point_light (point 0. 10. (-10.)) (white ()) in
  let result = lighting m light pos eyev normalv false in
  assert_bool "eye in path" (color_equal {r=1.6364; g=1.6364; b=1.6364} result)

let test_light_behind_surface _ =
  let m = default_material () in
  let pos = point 0. 0. 0. in
  let eyev = vector 0. 0. (-1.) in
  let normalv = vector 0. 0. (-1.) in
  let light = point_light (point 0. 0. (10.)) (white ()) in
  let result = lighting m light pos eyev normalv false in
  assert_equal {r=0.1; g=0.1; b=0.1} result

let test_light_in_shadow _ =
  let m = default_material () in
  let pos = point 0. 0. 0. in
  let eyev = vector 0. 0. (-1.) in
  let normalv = vector 0. 0. (-1.) in
  let light = point_light (point 0. 0. (-10.)) (white ()) in
  let result = lighting m light pos eyev normalv true in
  assert_equal {r=0.1; g=0.1; b=0.1} result  

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
  ]

let () =
  run_test_tt_main suite