open OUnit2
open Tuple
open Matrices
open Rays
open Sphere


let test_ray_creation_and_querying _ =
  let origin = {x=1.; y=2.; z=3.; w=1.;} in
  let direction = {x=4.; y=5.; z=6.; w=0.;} in
  let r = {origin=origin; direction=direction} in
  assert_equal origin r.origin;
  assert_equal direction r.direction

let test_ray_position _ =
  let r = {origin={x=2.; y=3.; z=4.; w=1.;}; direction={x=1.; y=0.; z=0.; w=0.;}} in
  let p1_correct = {x=2.; y=3.; z=4.; w=1.;} in
  let p2_correct = {x=3.; y=3.; z=4.; w=1.;} in
  let p3_correct = {x=1.; y=3.; z=4.; w=1.;} in
  let p4_correct = {x=4.5; y=3.; z=4.; w=1.;} in
  let p1 = position r 0. in
  let p2 = position r 1. in
  let p3 = position r (-1.) in
  let p4 = position r 2.5 in
  assert_equal p1_correct p1;
  assert_equal p2_correct p2;
  assert_equal p3_correct p3;
  assert_equal p4_correct p4

let test_ray_intersects_sphere_twice _ =
  let r = {origin={x=0.; y=0.; z=(-5.); w=1.;}; direction={x=0.; y=0.; z=1.; w=0.;}} in
  let s = new sphere in
  let xs = intersect s r in
  assert_equal 2 (List.length xs);
  assert_equal 4.0  (List.nth xs 0).t;
  assert_equal 6.0 (List.nth xs 1).t


let test_ray_intersects_sphere_tangent _ =
  let r = {origin={x=0.; y=1.; z=(-5.); w=1.;}; direction={x=0.; y=0.; z=1.; w=0.;}} in
  let s = new sphere in
  let xs = intersect s r in
  (* print_intersections xs; *)
  assert_equal 2 (List.length xs);
  assert_equal 5.0  (List.nth xs 0).t;
  assert_equal 5.0 (List.nth xs 1).t

let test_ray_misses _ =
  let r = {origin={x=0.; y=2.; z=(-5.); w=1.;}; direction={x=0.; y=0.; z=1.; w=0.;}} in
  let s = new sphere in
  let xs = intersect s r in
  assert_equal 0  (List.length xs)

let test_ray_origin_inside_sphere _ =
  let r = {origin={x=0.; y=0.; z=(0.); w=1.;}; direction={x=0.; y=0.; z=1.; w=0.;}} in
  let s = new sphere in
  let xs = intersect s r in
  assert_equal 2 (List.length xs);
  assert_equal (-1.0)  (List.nth xs 0).t;
  assert_equal 1.0 (List.nth xs 1).t

let test_ray_in_front_of_sphere _ =
  let r = {origin={x=0.; y=0.; z=(5.); w=1.;}; direction={x=0.; y=0.; z=1.; w=0.;}} in
  let s = new sphere in
  let xs = intersect s r in
  (* print_intersections xs; *)
  assert_equal 2 (List.length xs);
  assert_equal (-6.0) (List.nth xs 0).t;
  assert_equal (-4.0) (List.nth xs 1).t

let suite =
  "RaysTestList" >::: [
    "test_ray_creation_and_querying" >:: test_ray_creation_and_querying;
    "test_ray_position" >:: test_ray_position;
    "test_ray_intersects_sphere_twice" >:: test_ray_intersects_sphere_twice;
    "test_ray_intersects_sphere_tangent" >:: test_ray_intersects_sphere_tangent;
    "test_ray_misses" >:: test_ray_misses;
    "test_ray_origin_inside_sphere" >:: test_ray_origin_inside_sphere;
    "test_ray_in_front_of_sphere" >:: test_ray_in_front_of_sphere;

  ]

let () =
  run_test_tt_main suite