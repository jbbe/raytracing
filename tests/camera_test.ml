open OUnit2
open Raytracing.Camera
open Raytracing.Matrices
open Raytracing.Rays
open Raytracing.Transformations
open Raytracing.Tuple

let test_camera_construction _ =
  let hsize = 160 in
  let vsize = 120 in
  let fov = (pi /. 2.) in
  let c = new camera hsize vsize fov in
  assert_equal (pi /. 2.) c#fov;
  assert_equal 160 c#hsize;
  assert_equal 120 c#vsize;
  assert_equal (c#transform) identity_matrix

let test_pix_size_horiz _ =
  let c = new camera 200 125 (pi/.2.) in
  print_float c#pixel_size;
  assert_equal c#pixel_size 0.01

let test_pix_size_vert _ =
  let c = new camera  125 200 (pi/.2.) in
  assert_equal c#pixel_size 0.01

let test_ray_thru_center _ =
  let c = new camera 201 101 (pi/.2.) in
  let r = c#ray_for_pixel 100 50 in
  assert_equal r.origin (point 0. 0. 0.);
  assert_equal r.direction (vector 0. 0. (-1.))

let test_ray_thru_corner _ =
  let c = new camera 201 101 (pi/.2.) in
  let r = c#ray_for_pixel 0 0 in
  assert_equal r.origin (point 0. 0. 0.);
  assert_equal r.direction (vector 0.66519 0.33259 (-0.66851))

let test_ray_when_cam_is_transed _ =
  let deg45 = (sqrt 2.) /. 2. in
  let c = new camera 201 101 (pi/.2.) in
  c#set_transform (matrix_mult (rotation_y (pi /. 4.)) (translation  0. (-2.) 5.));
  let r = c#ray_for_pixel 100 50 in
  assert_equal r.origin (point 0. 2. (-5.));
  assert_equal r.direction (vector deg45 0. ((-1.) *. deg45))


let suite =
  "CameraTestList" >::: [
    "test_camera_construction" >:: test_camera_construction;
    "test_pix_size_horiz" >:: test_pix_size_horiz;
    "test_pix_size_vert" >:: test_pix_size_vert;
    "test_ray_thru_center" >:: test_ray_thru_center;
    "test_ray_thru_corner" >:: test_ray_thru_corner;
    "test_ray_when_cam_is_transed" >:: test_ray_when_cam_is_transed;

  ]

let () =
  run_test_tt_main suite