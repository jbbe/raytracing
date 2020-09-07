open OUnit2
open Canvas
open Color

let test_create_canvas _ =
  let c = new canvas 10 20 in
  assert_equal 10 c#height;
  assert_equal 20 c#width;
  assert_bool "pixels should be black" (color_equal {r=0.;g=0.;b=0.} (c#pixel 2 2))

let test_write_pixel _ =
  let c = new canvas 10 20 in
  c#write_pixel 1 2 {r=0.5;g=0.;b=1.};
  assert_bool "write pixel" (color_equal {r=0.5;g=0.;b=1.} (c#pixel 2 2))
 

let suite =
  "ColorTestList" >::: [
    "test_create_canvas" >:: test_create_canvas;
    "test_write_pixel" >:: test_write_pixel;
  ]

let () =
  run_test_tt_main suite