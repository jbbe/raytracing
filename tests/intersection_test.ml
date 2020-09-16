open OUnit2
open Tuple
open Color
open Matrices
open Rays
open Sphere
open Transformations
open Lights
open World


let precompute_state_of_intersection _ =
  let r = {origin=(point 0. 0. (-5.)) distance=(vector 0. 0. 1.)} in
  let shape = new sphere in
  let i = intersection 4 s in
  let comps = prepare_computations i r in
  assert_equal comps.t i.t;
  assert_equal comps.point (point 0. 0. (-1.));
  assert_equal comps.eyev (vector 0. 0. (-1.));
  assert_equal comps.normalv (vector 0. 0. (-1.))


let suite =
  "LightsList" >::: [
    "precompute_state_of_intersection" >:: precompute_state_of_intersection;

  ]

let () =
  run_test_tt_main suite