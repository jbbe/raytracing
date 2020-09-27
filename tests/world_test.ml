open OUnit2
open Raytracing.Tuple
open Raytracing.Color
open Raytracing.Rays
open Raytracing.Shapes
open Raytracing.Transformations
open Raytracing.Lights
open Raytracing.World
open Raytracing.Intersections

let test_create_world _ =
  let w = new world in
  assert_equal [] (w#objects);
  assert_equal [] (w#lights)

let test_default_world _ =
  let _light = point_light (point (-10.) 10. (-10.)) (white) in
  let s1 = new shape Sphere  in 
  let m1 = {ambient=0.1; 
            diffuse=0.7;
            specular=0.2;
          shininess=200.;
    transparency=0.0;
    refractive_idx=1.0;
    reflective=0.;
          pattern=new pattern Solid [{r=0.8; g=1.; b=0.6}]} in
  s1#set_material m1;
  let s2 = new shape Sphere  in
  let trans = scaling 0.5 0.5 0.5 in
  s2#set_transform trans;
  let w = default_world () in
  assert_equal w#lights [_light];
  let test_point = point 0. 0. 1. in
  (* print_spheres w#objects;
  l
  print_spheres ([s1;s2]); *)
  assert_equal 2 (List.length (w#objects));
  assert_equal m1.shininess (((List.nth w#objects 0)#material)).shininess;
  assert_equal m1.specular (((List.nth w#objects 0)#material)).specular;
  assert_equal m1.ambient (((List.nth w#objects 0)#material)).ambient;
  assert_equal (m1.pattern#color_at test_point) (((List.nth w#objects 0)#material).pattern#color_at test_point);
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
  let s = (List.hd w#objects ) in
  let i =  {t=4.;obj=(ref s)} in
  let comps = prepare_computations i r [i] in
  let c = w#shade_hit comps in
  (* print_color c; *)
  assert_bool "shading test 1" (color_equal c {r=0.38066; g=0.47583; b=0.2855;})

let test_shadeing_intersection_from_inside _ =
  let w = default_world () in
  w#add_light (point_light (point 0. 0.25 0.) {r=1.; g=1.; b=1.});
  let r = {origin=(point 0. 0. (0.)); direction=(vector 0. 0. 1.)} in
  let s = (List.nth w#objects 1) in
  let i =  {t=0.5; obj=(ref s)} in
  let comps = prepare_computations i r [i] in
  let c = w#shade_hit comps  in
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
  assert_equal (w#color_at r) (inner#color_at (point 0. 0. 0.)) 

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
  w#add_light (point_light (point 0. 0. (-10.)) (white));
  w#add_object (new shape Sphere );
  let s2 = new shape Sphere  in
  s2#set_transform (translation 0. 0. 10.);
  w#add_object s2;
  let r = {origin=(point 0. 0. 5.); direction=(vector 0. 0. 1.)} in
  let i = {t=4.; obj=(ref s2)} in
  let comps = prepare_computations i r [i] in
  let c = w#shade_hit comps in
  assert_equal c ({r=0.1; g=0.1; b=0.1})


let test_nonreflective_reflection _ =
  let w = default_world () in
  let r = {origin=(point 0. 0. 0.); direction=(vector 0. 0. 1.)} in
  let s = List.nth (w#objects) 1 in
  s#material.ambient <- 1.;
  let i = {t=1.; obj=(ref s)} in
  let comps = prepare_computations i r [i] in
  let c = w#reflected_color comps in
  assert_equal black c

let test_reflected_color _ =
  let w = default_world () in
  let s = new shape Plane in
  s#material.reflective <- 0.5;
  s#set_transform (translation 0. (-1.) 0.);
  w#add_object s;
  let deg45 = (sqrt 2.) /. 2. in
  let r = {origin=(point 0. 0. (-3.)); direction=(vector 0. ((-1.) *. deg45) deg45)} in
  let i = {t=(sqrt 2.); obj=(ref s)} in
  let comps = prepare_computations i r [i] in
  (* Printf.printf "\n\ntest reflected color\n"; *)
  let c = w#reflected_color comps in
  (* Printf.printf "\n\nReflected color calculated ************\n"; *)

  (* print_computations comps; *)
  (* print_color c; *)
  (* w#print_world; *)
  assert_bool "reflecting" (color_equal {r=0.19032; g=0.2379; b=0.14274} c)

let test_shade_hit_with_reflective _ =
  let w = default_world () in
  let s = new shape Plane in
  s#material.reflective <- 0.5;
  s#set_transform (translation 0. (-1.) 0.);
  w#add_object s;
  let deg45 = (sqrt 2.) /. 2. in
  let r = {origin=(point 0. 0. (-3.)); direction=(vector 0. ((-1.) *. deg45) deg45)} in
  let i = {t=(sqrt 2.); obj=(ref s)} in
  let comps = prepare_computations i r [i] in
  (* Printf.printf "\n\ntest shade_hit with reflective begin shade_hit ************\n"; *)
  let c = w#shade_hit comps in
  (* Printf.printf "\n\ntest shade_hit with reflective returns ************\n"; *)
  (* print_color c; *)
  assert_bool "reflecting" (color_equal {r=0.87677; g=0.92436; b=0.82918} c) 

let test_recursion_ends _ =
  let w = new world in
  w#add_light (point_light (point 0. 0. 0.) white);
  let lower = new shape Plane in
  lower#material.reflective <- 1.;
  lower#set_transform (translation 0. (-1.) 0.);
  let upper = new shape Plane in
  upper#material.reflective <- 1.;
  upper#set_transform (translation 0. (1.) 0.);
  w#add_object lower;
  w#add_object upper;
  let r = {origin=(point 0. 0. 0.); direction=(vector 0. 1. 0.)} in
  let c = (w#color_at r) in
  assert_equal c c 

let test_reflection_in_the_depths _ =
  let w = default_world () in
  let s = new shape Plane in
  s#material.reflective <- 0.5;
  s#set_transform (translation 0. (-1.) 0.);
  w#add_object s;
  let deg45 = (sqrt 2.) /. 2. in
  let r = {origin=(point 0. 0. (-3.)); direction=(vector 0. ((-1.) *. deg45) deg45)} in
  let i = {t=(sqrt 2.); obj=(ref s)} in
  let comps = prepare_computations i r [i] in
  let c = w#reflected_color ~remaining:0 comps in
  print_color c;
  assert_equal c black

let test_refracting_opacity _ =
  let w = default_world () in
  let s = List.nth w#objects 0 in
  let r = {origin=(point 0. 0. (-5.)); direction=(vector 0. 0. 1.)} in
  let xs = [{t=4.; obj=(ref s)}; {t=6.; obj=(ref s)}] in
  let comps = prepare_computations (List.hd xs) r xs in
  let c = w#refracted_color comps  in
  assert_equal c black

let test_refracted_col_at_depth _ =
  let w = default_world () in
  let s = List.nth w#objects 0 in
  s#material.transparency <- 1.;
  s#material.refractive_idx <- 1.5;
  let r = {origin=(point 0. 0. (-5.)); direction=(vector 0. 0. 1.)} in
  let xs = [{t=4.; obj=(ref s)}; {t=6.; obj=(ref s)}] in
  let comps = prepare_computations (List.hd xs) r xs in
  let c = w#refracted_color ~remaining:0 comps in
  assert_equal c black

let test_total_internal_reflection _ =
  let w = default_world () in
  let s = List.nth w#objects 0 in
  s#material.transparency <- 1.;
  s#material.refractive_idx <- 1.5;
  let deg45 = (sqrt 2.) /. 2. in
  let r = {origin=(point 0. 0. deg45); direction=(vector 0. 1. 0.)} in
  let xs = [{t=((-1.) *. deg45); obj=(ref s)}; {t=deg45; obj=(ref s)}] in
  let comps = prepare_computations (List.nth xs 1) r xs in
  let c = w#refracted_color comps in
  assert_equal c black

let test_refract_general _ =
  let w = default_world () in
  let a = List.nth w#objects 0 in
  a#material.ambient  <- 1.;
  a#material.pattern <- new pattern Test [white;];
  let b = List.nth w#objects 1 in
  b#material.transparency  <- 1.;
  b#material.refractive_idx <- 1.5;
  let r = {origin=(point 0. 0. 0.1); direction=(vector 0. 1. 0.)} in
  let xs = [{t=(-0.9899) ; obj=(ref a)}; 
            {t=(-0.4899); obj=(ref b)}; 
            {t=(0.4899); obj=(ref b)};
            {t=(0.9899); obj=(ref a)};
            ] in
  let comps = prepare_computations (List.nth xs 2) r xs in
  let c = w#refracted_color comps in
  assert_bool "First reg refract test" (color_equal c {r=0.; g=0.99888; b=0.04725;})

let test_shade_hit_transparent _ =
    let w = default_world () in
    let floor = new shape Plane in
    floor#set_transform (translation 0. (-1.) 0.);
    floor#material.transparency <- 0.5;
    floor#material.refractive_idx <- 1.5;
    w#add_object floor;
    let ball = new shape Sphere in
    ball#material.pattern <- new pattern Solid [{r=1.;g=0.;b=0.}];
    ball#material.ambient <- 0.5;
    ball#set_transform (translation 0. (-3.5) (-0.5));
    w#add_object ball;
    let deg45 = (sqrt 2.) /. 2. in
    let r = {origin= (point 0. 0. (-3.)); direction=(vector 0. ((-1.) *. deg45) deg45)} in
    let xs =  [{t=(sqrt 2.); obj=(ref floor)}] in
    let comps = prepare_computations (List.hd xs) r xs in
    let c = w#shade_hit ~remaining:5 comps in
    assert_bool "shade_hit refract" (color_equal c {r=0.93642; g= 0.68642; b=0.68642})

let test_the_shlickness _ =
    let w = default_world () in
    let floor = new shape Plane in
    floor#set_transform (translation 0. (-1.) 0.);
    floor#material.reflective <- 0.5;
    floor#material.transparency <- 0.5;
    floor#material.refractive_idx <- 1.5;
    w#add_object floor;
    let ball = new shape Sphere in
    ball#material.pattern <- new pattern Solid [{r=1.;g=0.;b=0.}];
    ball#material.ambient <- 0.5;
    ball#set_transform (translation 0. (-3.5) (-0.5));
    w#add_object ball;
    let deg45 = (sqrt 2.) /. 2. in
    let r = {origin= (point 0. 0. (-3.)); direction=(vector 0. ((-1.) *. deg45) deg45)} in
    let xs =  [{t=(sqrt 2.); obj=(ref floor)}] in
    (* print_intersections xs; *)
    let comps = prepare_computations (List.hd xs) r xs in
    let c = w#shade_hit ~remaining:5 comps in
    print_color c;
    assert_bool "shade_hit schlickness" (color_equal c {r=0.93391; g= 0.69643; b=0.69243})
  

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
    "test_nonreflective_reflection" >:: test_nonreflective_reflection;
    "test_reflected_color" >:: test_reflected_color;
    "test_shade_hit_with_reflective" >:: test_shade_hit_with_reflective;
    "test_recursion_ends" >:: test_recursion_ends;
    "test_reflection_in_the_depths" >:: test_reflection_in_the_depths;
    "test_refracting_opacity" >:: test_refracting_opacity;
    "test_refracted_col_at_depth" >:: test_refracted_col_at_depth;
    "test_total_internal_reflection" >:: test_total_internal_reflection;
    "test_refract_general" >:: test_refract_general;
    "test_shade_hit_transparent" >:: test_shade_hit_transparent;
    "test_the_shlickness" >:: test_the_shlickness;

  ]

let () =
  run_test_tt_main suite