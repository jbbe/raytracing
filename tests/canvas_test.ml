open OUnit2
open Raytracing.Canvas
open Raytracing.Color

let correct_ppm = "P3
5 3
255
255 0 0 0 0 0 0 0 0 
0 0 0 0 0 0 0 0 0 
0 0 0 0 127 0 0 0 0 
0 0 0 0 0 0 0 0 0 
0 0 0 0 0 255 0 0 0 \n"

let correct_ppm2 = "P3\n10 2\n255\n255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204 \n153 255 204 153 255 204 153 255 204 153 255 204 153 \n255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204 \n153 255 204 153 255 204 153 255 204 153 255 204 153 \n"
(* let correct_ppm = "P3
5 3
255
255 0 0 0 0 0 0 0 0 
0 0 0 0 0 0 0 0 0 
0 0 0 0 127 0 0 0 0 
0 0 0 0 0 0 0 0 0 
0 0 0 0 0 255 0 0 0\n" *)
let test_create_canvas _ =
  let c = new canvas 10 20 in
  assert_equal 10 c#width;
  assert_equal 20 c#height;
  assert_bool "pixels should be black" (color_equal {r=0.;g=0.;b=0.} (c#pixel 2 2))

let test_write_pixel _ =
  let c = new canvas 10 20 in
  c#write_pixel 1 2 {r=0.5;g=0.;b=1.};
  assert_bool "write pixel 1" (color_equal {r=0.5;g=0.;b=1.} (c#pixel 1 2));
  assert_bool "Should be black" (color_equal {r=0.;g=0.;b=0.} (c#pixel 0 0));
  assert_bool "Should be black" (color_equal {r=0.;g=0.;b=0.} (c#pixel 0 2));
  assert_bool "Should be black" (color_equal {r=0.;g=0.;b=0.} (c#pixel 2 0))



let test_canvas_to_ppm _ =
  let c = new canvas 5 3 
  and c1 = {r=1.5;g=0.;b=0.}
  and c2 = {r=0.;g=0.5;b=0.}
  and c3 = {r=(-0.5);g=0.;b=1.} in
  c#write_pixel 0 0 c1;
  c#write_pixel 2 1 c2;
  c#write_pixel 4 1 c3;
  let ppm = c#to_ppm in
  (* Printf.printf "%s" ppm; *)
  assert_equal correct_ppm ppm

let test_long_ppm_line _ =
  let c = new canvas 10 2 in
  let ppm = c#to_ppm in
  Printf.printf "PPm\n%s" ppm;
  assert_equal correct_ppm2 ppm

let test_ppm_terminates _ =
  let c = new canvas 5 3 in
  let ppm = c#to_ppm in
  let len = String.length ppm in
  let last_char = String.get ppm (len - 1) in
  (* Printf.printf "PPm\n%s" ppm;
  Printf.printf "Char %d %d" (Char.code last_char) len; *)
  assert_equal 10 (Char.code last_char) 


let suite =
  "CanvasTestList" >::: [
    "test_create_canvas" >:: test_create_canvas;
    "test_write_pixel" >:: test_write_pixel;
    "test_canvas_to_ppm" >:: test_canvas_to_ppm;
    (* "test_long_ppm_line" >:: test_long_ppm_line; *)
    "test_ppm_terminates" >:: test_ppm_terminates;
  ]

let () =
  run_test_tt_main suite