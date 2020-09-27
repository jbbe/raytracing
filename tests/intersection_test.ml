open OUnit2
open Raytracing.Tuple
open Raytracing.Rays
open Raytracing.Shapes
open Raytracing.Intersections
open Raytracing.Transformations


let precompute_state_of_intersection _ =
  let r = {origin=(point 0. 0. (-5.)); direction=(vector 0. 0. 1.)} in
  let s = new shape Sphere  in
  let i = {t=4.; obj=(ref s)} in
  let comps = prepare_computations i r [i] in
  assert_equal comps.t i.t;
  assert_equal comps.point (point 0. 0. (-1.));
  assert_equal comps.eyev (vector 0. 0. (-1.));
  assert_equal comps.normalv (vector 0. 0. (-1.))

let interection_occurs_on_outside _ =
  let r = {origin=(point 0. 0. (-5.)); direction=(vector 0. 0. 1.)} in
  let s = new shape Sphere  in
  let i = {t=4.; obj=(ref s)} in
  let comps = prepare_computations i r [i] in
  assert_equal comps.t i.t;
  assert_equal comps.inside false
  
let interection_occurs_on_inside _ =
  let r = {origin=(point 0. 0. (0.)); direction=(vector 0. 0. 1.)} in
  let s= new shape Sphere  in
  let i = {t=1.; obj=(ref s)} in
  let comps = prepare_computations i r [i] in
  assert_equal comps.t i.t;
  assert_equal comps.point (point 0. 0. (1.));
  assert_equal comps.eyev (vector 0. 0. (-1.));
  assert_equal comps.normalv (vector 0. 0. (-1.));
  assert_equal comps.inside true

let test_acne_free _ =
  let r = {origin=(point 0. 0. (-5.)); direction=(vector 0. 0. 1.)} in
  let s = new shape Sphere  in
  s#set_transform (translation 0. 0. 1.);
  let i = {t=5.; obj=(ref s)} in
  let comps = prepare_computations i r [i] in
  assert_bool "over by less than epsilon over 2" (comps.over_point.z < (_EPSILON /. (-2.)));
  assert_bool "over by less than epsilon over 2" (comps.point.z > comps.over_point.z)

let test_comp_reflectv _ =
  let s = new shape Plane in
  let deg45 = (sqrt 2.) /. 2. in
  let r = {origin=(point 0. 1. (-1.)); direction=(vector 0. ((-1.) *. deg45) deg45)} in
  let i = {t=deg45; obj=(ref s)} in
  let comps = prepare_computations i r [i] in
  assert_equal (vector 0. deg45 deg45) comps.reflectv

let test_finding_n1_n2 _ =
  let a = glass_sphere () in
  a#set_transform (scaling 2. 2. 2.);
  a#material.refractive_idx <-1.5;
  let b = glass_sphere () in
  b#set_transform (translation 0. 0. (-0.25));
  b#material.refractive_idx <-2.0;
  let c = glass_sphere () in
  c#set_transform (translation 0. 0. (0.25));
  c#material.refractive_idx <-2.5;
  let r = {origin=(point 0. 0. (-4.)); direction=(vector 0. 0. 1.)} in
  let xs = [{t=2.;obj=(ref a)}; 
     {t=2.75; obj=(ref b)}; 
      {t=3.25; obj=(ref c)}; 
      {t=4.75; obj=(ref b)}; 
      {t=5.25;obj=(ref c)}; 
      {t=6.; obj=(ref a)};] in
  let xs0 = {t=2.;obj=(ref a)};  in
  let xs1 =           {t=2.75; obj=(ref b)}; in
  let xs2 =            {t=3.25; obj=(ref c)}; in
  let xs3 =            {t=4.75; obj=(ref b)}; in
  let xs4 =            {t=5.25;obj=(ref c)}; in
  let xs5 =            {t=6.; obj=(ref a)}; in
  let c0 = prepare_computations xs0 r xs in
  assert_equal 1.0 c0.n1;
  assert_equal 1.5 c0.n2;
  let c1 = prepare_computations xs1 r xs in
  assert_equal 1.5 c1.n1;
  assert_equal 2.0 c1.n2;
  let c2 = prepare_computations xs2 r xs in
  assert_equal 2.0 c2.n1;
  assert_equal 2.5 c2.n2;
  let c3 = prepare_computations xs3 r xs in
  assert_equal 2.5 c3.n1;
  assert_equal 2.5 c3.n2;
  let c4 = prepare_computations xs4 r xs in
  assert_equal 2.5 c4.n1;
  assert_equal 1.5 c4.n2;
  let c5 = prepare_computations xs5 r xs in
  assert_equal 1.5 c5.n1;
  assert_equal 1.0 c5.n2

let test_underpoint_below_surface _ =
  let r = {origin=(point 0. 0. (-5.)); direction=(vector 0. 0. (1.))} in
  let s = glass_sphere () in
  s#set_transform (translation 0. 0. 1.);
  let i =  {t=5.; obj= (ref s)} in
  let xs = [i] in
  let comps = prepare_computations i r xs in
  assert_bool "underpoint less than eps" (comps.under_point.z > (_EPSILON /. 2.));
  assert_bool "underpoint below point" (comps.point.z < comps.under_point.z)

let test_shlick_total _ =
  let s = glass_sphere () in
  let deg45 = (sqrt 2.) /. 2. in
  let negdeg = (-1.) *. deg45 in
  let r = {origin= (point 0. 0. deg45); direction=(vector 0. 1. 0.)} in
  let xs =  [{t=negdeg; obj=(ref s)}; {t=deg45; obj=(ref s)}] in
  let comps = prepare_computations (List.nth xs 1) r xs in
  let reflectance = schlick comps in
  assert_equal 1. reflectance

let test_perpendicular_reflectance _ =
  let s = glass_sphere () in
  let r = {origin= (point 0. 0. 0.); direction=(vector 0. 1. 0.)} in
  let xs =  [{t=(-1.); obj=(ref s)}; {t=1.; obj=(ref s)}] in
  let comps = prepare_computations (List.nth xs 1) r xs in
  let reflectance = schlick comps in
  assert_bool "perpendicular" (( Float.abs (0.04 -. reflectance )) < _EPSILON)

let test_reflectance_n2_g_n1 _ =
  let s = glass_sphere () in
  let r = {origin= (point 0. 0.99 (-2.)); direction=(vector 0. 0. 1.)} in
  let xs =  [{t=(1.8589); obj=(ref s)};] in
  let comps = prepare_computations (List.nth xs 0) r xs in
  let reflectance = schlick comps in
  assert_bool "refl"  (( Float.abs(0.48873 -. reflectance )) < _EPSILON)

let test_cast_shadow _ =
  let s = glass_sphere () in
  s#set_casts_shadow false;
  let xs =  [{t=(1.8589); obj=(ref s)};] in
  assert_equal false (intersection_casts_shadow xs 20.)

let test_cast_shadow_2 _ =
  let s = glass_sphere () in
  let xs =  [{t=(1.8589); obj=(ref s)};] in
  assert_equal true (intersection_casts_shadow xs 20.)

let test_cast_shadow_long_list_true _ =
  let a = glass_sphere () in
  a#set_transform (scaling 2. 2. 2.);
  a#material.refractive_idx <-1.5;
  let b = glass_sphere () in
  b#set_casts_shadow false;
  b#set_transform (translation 0. 0. (-0.25));
  b#material.refractive_idx <-2.0;
  let c = glass_sphere () in
  c#set_casts_shadow false;
  (* let r = {origin=(point 0. 0. (-4.)); direction=(vector 0. 0. 1.)} in *)
  let xs = [{t=2.;obj=(ref a)}; 
     {t=2.75; obj=(ref b)}; 
      {t=3.25; obj=(ref c)}; 
      {t=4.75; obj=(ref b)}; 
      {t=5.25;obj=(ref c)}; 
      {t=6.; obj=(ref a)};] in
  assert_equal true (intersection_casts_shadow xs 20.)

let test_cast_shadow_long_list_false _ =
  let a = glass_sphere () in
  a#set_transform (scaling 2. 2. 2.);
  a#material.refractive_idx <-1.5;
  let b = glass_sphere () in
  b#set_casts_shadow false;
  a#set_casts_shadow false;
  b#set_transform (translation 0. 0. (-0.25));
  b#material.refractive_idx <-2.0;
  let c = glass_sphere () in
  c#set_casts_shadow false;
  (* let r = {origin=(point 0. 0. (-4.)); direction=(vector 0. 0. 1.)} in *)
  let xs = [{t=2.;obj=(ref a)}; 
     {t=2.75; obj=(ref b)}; 
      {t=3.25; obj=(ref c)}; 
      {t=4.75; obj=(ref b)}; 
      {t=5.25;obj=(ref c)}; 
      {t=6.; obj=(ref a)};] in
  assert_equal false (intersection_casts_shadow xs 20.)

let suite =
  "LightsList" >::: [
    "precompute_state_of_intersection" >:: precompute_state_of_intersection;
    "interection_occurs_on_outside" >:: interection_occurs_on_outside;
    "interection_occurs_on_inside" >:: interection_occurs_on_inside;
    "test_acne_free" >:: test_acne_free;
    "test_comp_reflectv" >:: test_comp_reflectv;
    "test_finding_n1_n2" >:: test_finding_n1_n2;
    "test_underpoint_below_surface" >:: test_underpoint_below_surface;
    "test_shlick_total" >:: test_shlick_total;
    "test_perpendicular_reflectance" >:: test_perpendicular_reflectance;
    "test_reflectance_n2_g_n1" >:: test_reflectance_n2_g_n1;
    "test_cast_shadow" >:: test_cast_shadow;
    "test_cast_shadow_2" >:: test_cast_shadow_2;
    "test_cast_shadow_long_list_true" >:: test_cast_shadow_long_list_true;
    "test_cast_shadow_long_list_false" >:: test_cast_shadow_long_list_false;

  ]

let () =
  run_test_tt_main suite