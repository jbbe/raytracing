open OUnit2
open Raytracing.Tuple
open Raytracing.Rays
open Raytracing.Sphere
open Raytracing.Intersections


let precompute_state_of_intersection _ =
  let r = {origin=(point 0. 0. (-5.)); direction=(vector 0. 0. 1.)} in
  let shape = new sphere in
  let i = {t=4.; obj=(ref shape)} in
  let comps = prepare_computations i r in
  assert_equal comps.t i.t;
  assert_equal comps.point (point 0. 0. (-1.));
  assert_equal comps.eyev (vector 0. 0. (-1.));
  assert_equal comps.normalv (vector 0. 0. (-1.))

let interection_occurs_on_outside _ =
  let r = {origin=(point 0. 0. (-5.)); direction=(vector 0. 0. 1.)} in
  let shape = new sphere in
  let i = {t=4.; obj=(ref shape)} in
  let comps = prepare_computations i r in
  assert_equal comps.t i.t;
  assert_equal comps.inside false

  
let interection_occurs_on_inside _ =
  let r = {origin=(point 0. 0. (0.)); direction=(vector 0. 0. 1.)} in
  let shape = new sphere in
  let i = {t=1.; obj=(ref shape)} in
  let comps = prepare_computations i r in
  assert_equal comps.t i.t;
  assert_equal comps.point (point 0. 0. (1.));
  assert_equal comps.eyev (vector 0. 0. (-1.));
  assert_equal comps.normalv (vector 0. 0. (-1.));
  assert_equal comps.inside true

let suite =
  "LightsList" >::: [
    "precompute_state_of_intersection" >:: precompute_state_of_intersection;
    "interection_occurs_on_outside" >:: interection_occurs_on_outside;
    "interection_occurs_on_inside" >:: interection_occurs_on_inside;

  ]

let () =
  run_test_tt_main suite