open OUnit2
open Color

let test_color_def _ =
  let c = {r= (-0.5); g=0.4; b=1.7} in
  assert_equal (-0.5) c.r;
  assert_equal (0.4) c.g;
  assert_equal (1.7) c.b

let suite =
  "ColorTestList" >::: [
    "test_color_def" >:: test_color_def;
  ]

let () =
  run_test_tt_main suite