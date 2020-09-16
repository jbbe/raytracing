open OUnit2
open Raytracing.Color

let test_color_def _ =
  let c = {r= (-0.5); g=0.4; b=1.7} in
  assert_equal (-0.5) c.r;
  assert_equal (0.4) c.g;
  assert_equal (1.7) c.b

let test_color_ops _ =
  let c1 = {r=0.9; g=0.6; b=0.75}
  and c2 = {r=0.7; g=0.1; b=0.25} in
  assert_bool "color add" (color_equal {r=1.6; g=0.7; b=1.0} (color_add c1 c2));
  assert_bool "color sub" (color_equal {r=0.2; g=0.5; b=0.5} (color_sub c1 c2))

let test_color_equal _ =
  let c1 = {r=0.9; g=0.6; b=0.75}
  and c2 = {r=0.7; g=0.1; b=0.25} in
  assert_bool "same" (color_equal c1 c1);
  assert_bool "same" (color_equal c2 c2);
  assert_bool "un equal" (not (color_equal c1 c2))

let test_scalar_color_mult _ =
  let c = {r=0.2; g=0.3; b=0.4} in
  assert_bool "scalar color mult" (color_equal {r=0.4; g=0.6; b=0.8} (color_scalar_mult c 2.))

let test_color_mult _ =
  let c1 = {r=1.; g=0.2; b=0.4}
  and c2 = {r=0.9; g=1.; b=0.1} in
  assert_bool "color mult" (color_equal {r=0.9; g=0.2; b=0.04} (schur_prod c1 c2))

let suite =
  "ColorTestList" >::: [
    "test_color_def" >:: test_color_def;
    "test_color_ops" >:: test_color_ops;
    "test_scalar_color_mult" >:: test_scalar_color_mult;
    "test_color_mult" >:: test_color_mult;
  ]

let () =
  run_test_tt_main suite