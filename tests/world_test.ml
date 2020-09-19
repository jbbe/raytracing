open OUnit2
open Raytracing.Tuple
open Raytracing.Color
open Raytracing.Rays
open Raytracing.Sphere
open Raytracing.Transformations
open Raytracing.Lights
open Raytracing.World
open Raytracing.Intersections

let test_create_world _ =
  let w = new world in
  assert_equal [] (w#objects);
  assert_equal [] (w#lights)

let test_default_world _ =
  let _light = point_light (point (-10.) 10. (-10.)) (white ()) in
  let s1 = new sphere in 
  let m1 = {color={r=0.8; g=1.; b=0.6};
            ambient=0.1; 
            diffuse=0.7;
            specular=0.2;
          shininess=200.} in
  s1#set_material m1;
  let s2 = new sphere in
  let trans = scaling 0.5 0.5 0.5 in
  s2#set_transform trans;
  let w = default_world () in
  assert_equal w#lights [_light];
  (* print_spheres w#objects;
  print_spheres ([s1;s2]); *)
  assert_equal 2 (List.length (w#objects));
  assert_equal m1 (((List.nth w#objects 0)#material));
  assert_equal trans (((List.nth w#objects 1)#transform))
  (* assert_bool "s2 in w" (((List.nth w#objects 1)#transform) =  trans) *)


let world_ray_intersec _ =
  let w = default_world () in
  let r =  {origin=(point 0. 0. (-5.)); direction=(vector 0. 0. 1.)} in
  (* print_spheres (w#objects); *)
  let xs = (w#intersect r) in
  assert_equal 4 (List.length xs);
  assert_equal (4.0)  (List.nth xs 0).t;
  assert_equal 4.5 (List.nth xs 1).t;
  assert_equal 5.5 (List.nth xs 2).t;
  assert_equal 6.0 (List.nth xs 3).t

let test_shadeing_intersection _ =
  let w = default_world () in
  let r = {origin=(point 0. 0. (-5.)); direction=(vector 0. 0. 1.)} in
  let shape = (List.hd w#objects ) in
  let i =  {t=4.;obj=(ref shape)} in
  let comps = prepare_computations i r in
  let c = w#shade_hit comps in
  (* print_color c; *)
  assert_bool "shading test 1" (color_equal c {r=0.38066; g=0.47583; b=0.2855;})

let test_shadeing_intersection_from_inside _ =
  let w = default_world () in
  w#add_light (point_light (point 0. 0.25 0.) {r=1.; g=1.; b=1.});
  let r = {origin=(point 0. 0. (0.)); direction=(vector 0. 0. 1.)} in
  let shape = (List.nth w#objects 1) in
  let i =  {t=0.5; obj=(ref shape)} in
  let comps = prepare_computations i r in
  let c = w#shade_hit comps in
  (* print_color c; *)
  assert_bool "shading test 2" (color_equal c {r=0.90498; g=0.90498; b=0.90498;})

let test_color_when_ray_misses _ =
  let w = default_world () in
  let r = {origin=(point 0. 0. (-5.)); direction=(vector 0. 1. 0.)} in
  let correct = {r=0.; g=0.; b=0.;} in
  assert_equal (w#color_at r) correct

let test_color_on_hit _ =
  let w = default_world () in
  let r = {origin=(point 0. 0. (-5.)); direction=(vector 0. 0. 1.)} in
  let correct = {r=0.38066; g=0.47583; b=0.2855;} in
  assert_bool "color on hit" (color_equal (w#color_at r) correct)

let test_color_given_intersection_behind_ray _ =
  let w = default_world () in
  let r = {origin=(point 0. 0. (0.75)); direction=(vector 0. 0. (-1.))} in
  let outer = (List.nth w#objects 0) in
  outer#material.ambient <- 1.;
  let inner = (List.nth w#objects 1) in
  inner#material.ambient <- 1.;
  (* let correct = {r=0; g=0.; b=0.;} in *)
  assert_equal (w#color_at r) (inner#material.color) 

let test_no_shadow_collinear _ =
  let w = default_world () in
  let p = point 0. 10. 0. in
  assert_equal false (w#is_shadowed p)

let test_shadow_ed _ =
  let w = default_world () in
  let p = point 10. (-10.) 10. in
  assert_equal true (w#is_shadowed p)

let test_no_shadow_obj_behind_light _ =
  let w = default_world () in
  let p = point (-20.) 20. (-20.) in
  assert_equal false (w#is_shadowed p)

let test_no_shadow_obj_behind_point _ =
  let w = default_world () in
  let p = point (-2.) 2. (-2.) in
  assert_equal false (w#is_shadowed p)

let test_shade_hit_intersection_in_shadow _ =
  let w = new world in
  w#add_light (point_light (point 0. 0. (-10.)) (white ()));
  w#add_object (new sphere);
  let s2 = new sphere in
  s2#set_transform (translation 0. 0. 10.);
  w#add_object s2;
  let r = {origin=(point 0. 0. 5.); direction=(vector 0. 0. 1.)} in
  let i = {t=4.; obj=(ref s2)} in
  let comps = prepare_computations i r in
  let c = w#shade_hit comps in
  assert_equal c ({r=0.1; g=0.1; b=0.1})



let suite =
  "WorldList" >::: [
    "test_create_world" >:: test_create_world;
    "test_default_world" >:: test_default_world;
    "world_ray_intersec" >:: world_ray_intersec;
    "test_shadeing_intersection" >:: test_shadeing_intersection;
    "test_shadeing_intersection_from_inside" >:: test_shadeing_intersection_from_inside;
    "test_color_when_ray_misses" >:: test_color_when_ray_misses;
    "test_color_on_hit" >:: test_color_on_hit;
    "test_color_given_intersection_behind_ray" >:: test_color_given_intersection_behind_ray;
    "test_no_shadow_collinear" >:: test_no_shadow_collinear;
    "test_shadow_ed" >:: test_shadow_ed;
    "test_no_shadow_obj_behind_light" >:: test_no_shadow_obj_behind_light;
    "test_no_shadow_obj_behind_point" >:: test_no_shadow_obj_behind_point;
    "test_shade_hit_intersection_in_shadow" >:: test_shade_hit_intersection_in_shadow;

  ]

let () =
  run_test_tt_main suite