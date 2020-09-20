open OUnit2
open Raytracing.Tuple
open Raytracing.Matrices
open Raytracing.Shapes
open Raytracing.Transformations

let test_sphere_normal_x _ =
  let s = new shape Sphere in
  let n = s#normal_at (point 1. 0. 0.) in
  assert_equal (vector 1. 0. 0.) n
 
let test_sphere_normal_y _ =
  let s = new shape Sphere  in
  let n = s#normal_at (point 0. 1. 0.) in
  assert_equal (vector 0. 1. 0.) n

let test_sphere_normal_z _ =
  let s = new shape Sphere  in
  let n = s#normal_at (point 0. 0. 1.) in
  assert_equal (vector 0. 0. 1.) n

let test_sphere_normal_non_axial _ =
  let s = new shape Sphere  in
  let t = (sqrt 3.) /. 3. in
  let n = s#normal_at (point t t t) in
  assert_equal (vector t t t) n;
  assert_equal n (normalize n)

let test_normal_post_translation _ =
  let s = new shape Sphere  in
  s#set_transform (translation 0. 1. 0.);
  let n = s#normal_at (point 0. 1.70711 (-0.70711)) in
  assert_bool "test normal post translation" (tuple_equal (vector 0. 0.70711 (-0.70711)) n)

let test_normal_post_transform _ =
  let s = new shape Sphere  in
  let m = matrix_mult (scaling 1. 0.5 1.) (rotation_z (pi /. 5.)) in
  s#set_transform m;
  let t = (sqrt 2.) /. 2. in
  let n = s#normal_at (point 0. t (-1. *. t)) in
  assert_bool "normal post transform" (tuple_equal (vector 0. 0.97014 (-0.24254)) n)


let suite =
  "SphereList" >::: [
    "test_sphere_normal_x" >:: test_sphere_normal_x;
    "test_sphere_normal_y" >:: test_sphere_normal_y;
    "test_sphere_normal_z" >:: test_sphere_normal_z;
    "test_sphere_normal_non_axial" >:: test_sphere_normal_non_axial;
    "test_normal_post_translation" >:: test_normal_post_translation;
    "test_normal_post_transform" >:: test_normal_post_transform;

  ]

let () =
  run_test_tt_main suite