open OUnit2
open Raytracing.Intersections
open Raytracing.Rays
open Raytracing.Shapes
open Raytracing.Tuple



let test_plane_normality _ =
  let p = new shape Plane in
  let correct = vector 0. 1. 0. in
  let n1 = p#normal_at (point 0. 0. 0.) in
  let n2 = p#normal_at (point 10. 0. (-150.)) in
  let n3 = p#normal_at (point (-5.) 0. 150.) in
  assert_equal correct n1;
  assert_equal correct n2;
  assert_equal correct n3

let test_parallel_intersect_plane _ =
  let p = new shape Plane in
  let r = {origin=(point 0. 10. 0.); direction=(vector 0. 0. 1.)} in
  let xs = intersect p r in
  assert_equal [] xs


let test_coplanar_intersect _ =
  let p = new shape Plane in
  let r = {origin=(point 0. 0. 0.); direction=(vector 0. 0. 1.)} in
  let xs = intersect p r in
  assert_equal [] xs

let test_ray_intersects_plane_from_above _ =
  let p = new shape Plane in
  let r = {origin=(point 0. 1. 0.); direction=(vector 0. (-1.) 0.)} in
  let xs = intersect p r in
  assert_equal 1 (List.length xs);
  assert_equal 1. ((List.nth xs 0).t);
  assert_equal (ref p) (List.nth xs 0).obj

let test_ray_intersects_plane_from_below _ =
  let p = new shape Plane in
  let r = {origin=(point 0. (-1.) 0.); direction=(vector 0. (1.) 0.)} in
  let xs = intersect p r in
  assert_equal 1 (List.length xs);
  assert_equal 1. ((List.nth xs 0).t);
  assert_equal (ref p) (List.nth xs 0).obj

let suite =
  "LightsList" >::: [
    "test_plane_normality" >:: test_plane_normality;
    "test_parallel_intersect_plane" >:: test_parallel_intersect_plane;
    "test_coplanar_intersect" >:: test_coplanar_intersect;
    "test_ray_intersects_plane_from_above" >:: test_ray_intersects_plane_from_above;
    "test_ray_intersects_plane_from_below" >:: test_ray_intersects_plane_from_below;

  ]

let () =
  run_test_tt_main suite