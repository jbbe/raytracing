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
  let comps = prepare_computations i r in
  assert_equal comps.t i.t;
  assert_equal comps.point (point 0. 0. (-1.));
  assert_equal comps.eyev (vector 0. 0. (-1.));
  assert_equal comps.normalv (vector 0. 0. (-1.))

let interection_occurs_on_outside _ =
  let r = {origin=(point 0. 0. (-5.)); direction=(vector 0. 0. 1.)} in
  let s = new shape Sphere  in
  let i = {t=4.; obj=(ref s)} in
  let comps = prepare_computations i r in
  assert_equal comps.t i.t;
  assert_equal comps.inside false
  
let interection_occurs_on_inside _ =
  let r = {origin=(point 0. 0. (0.)); direction=(vector 0. 0. 1.)} in
  let s= new shape Sphere  in
  let i = {t=1.; obj=(ref s)} in
  let comps = prepare_computations i r in
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
  let comps = prepare_computations i r in
  assert_bool "over by less than epsilon over 2" (comps.over_point.z < (_EPSILON /. (-2.)));
  assert_bool "over by less than epsilon over 2" (comps.point.z > comps.over_point.z)



let suite =
  "LightsList" >::: [
    "precompute_state_of_intersection" >:: precompute_state_of_intersection;
    "interection_occurs_on_outside" >:: interection_occurs_on_outside;
    "interection_occurs_on_inside" >:: interection_occurs_on_inside;
    "test_acne_free" >:: test_acne_free;

  ]

let () =
  run_test_tt_main suite