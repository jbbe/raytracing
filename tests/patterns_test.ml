open OUnit2
open Raytracing.Color
open Raytracing.Patterns

let w = white 
let b = black 

let test_stripe_pattern _ =
  let p = stripe_pattern white  black  in
  assert_equal p.a w;
  assert_equal p.b b

let test_stripe_pattern_constant_in_y _ =
  let p = stripe_pattern white  black  in
  assert_equal w (stripe_at p (point 0. 0. 0.));
  assert_equal w (stripe_at p (point 0. 1. 0.));
  assert_equal w (stripe_at p (point 0. 2. 0.))

let test_stripe_pattern_constant_in_z _ =
  let p = stripe_pattern (white) (black) in
  assert_equal w (stripe_at p (point 0. 0. 0.));
  assert_equal w (stripe_at p (point 0. 0. 1.));
  assert_equal w (stripe_at p (point 0. 2. 2.))


let test_stripe_pattern_alternates_in_x _ =
  let p = stripe_pattern (white) (black) in
  assert_equal w (stripe_at p (point 0. 0. 0.));
  assert_equal w (stripe_at p (point 0.9 0. 0.));
  assert_equal b (stripe_at p (point 1. 0. 0.));
  assert_equal b (stripe_at p (point 1. 0. 0.));
  assert_equal b (stripe_at p (point (-0.1) 0. 0.));
  assert_equal b (stripe_at p (point (-1.) 0. 0.));
  assert_equal 2 (stripe_at p (point (-1.1) 0. 0.))

let suite =
  "LightsList" >::: [
    "test_stripe_pattern" >:: test_stripe_pattern;
    "test_stripe_pattern_constant_in_y" >:: test_stripe_pattern_constant_in_y;
    "test_stripe_pattern_constant_in_z" >:: test_stripe_pattern_constant_in_z;
    "test_stripe_pattern_alternates_in_x" >:: test_stripe_pattern_alternates_in_x;
    "test_stripe_pattern" >:: test_stripe_pattern;

  ]

let () =
  run_test_tt_main suite