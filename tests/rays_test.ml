open OUnit2
open Tuple
open Matrices
open Rays
open Sphere
open Transformations

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

let test_xs_encapsulates_t_and_object _ =
  let s = new sphere in
  let i = {t=3.5; obj=s#id} in
  assert_equal s#id i.obj

let test_aggregating_intersections _ =
  let s = new sphere in
  let i1 = {t=1.; obj=s#id} in
  let i2 = {t=2.; obj=s#id} in
  let xs = [i1; i2] in
  assert_equal 2 (List.length xs);
  assert_equal (1.)  (List.nth xs 0).t;
  assert_equal 2. (List.nth xs 1).t

let test_hit_all_pos _ =
  let s = new sphere in
  let i1 = {t=1.; obj=s#id} in
  let i2 = {t=2.; obj=s#id} in
  let xs = [i1; i2] in
  let i = hit xs in
  assert_equal i1 i

let test_hit_some_neg _ =
  let s = new sphere in
  let i1 = {t=(-1.); obj=s#id} in
  let i2 = {t=1.; obj=s#id} in
  let xs = [i1; i2] in
  let i = hit xs in
  assert_equal i2 i

let test_hit_all_neg _ =
  let s = new sphere in
  let i1 = {t=(-2.); obj=s#id} in
  let i2 = {t=(-1.); obj=s#id} in
  let xs = [i1; i2] in
  let i = hit xs in
  assert_equal null_x i

let test_hit_many _ =
  let s = new sphere in
  let i1 = {t=(5.); obj=s#id} in
  let i2 = {t=(7.); obj=s#id} in
  let i3 = {t=(-3.); obj=s#id} in
  let i4 = {t=(2.); obj=s#id} in
  let xs = [i1; i2; i3; i4] in
  let i = hit xs in
  assert_equal i4 i

let test_transform_ray _ =
  let r ={origin=(point 1. 2. 3.); direction= (vector 0. 1. 0.)} in
  let m = translation 3. 4. 5. in 
  let r2 = transform r m in
  assert_equal (point 4. 6. 8.) r2.origin;
  assert_equal (vector 0. 1. 0.) r2.direction

let test_scale_ray _ =
  let r ={origin=(point 1. 2. 3.); direction= (vector 0. 1. 0.)} in
  let m = scaling 2. 3. 4. in 
  let r2 = transform r m in
  assert_equal (point 2. 6. 12.) r2.origin;
  assert_equal (vector 0. 3. 0.) r2.direction

let test_sphere_default_trans _ =
  let s = new sphere in
  assert_equal s#transform identity_matrix

let test_change_sphere_trans _ =
  let s = new sphere in
  let t = translation 2. 3. 4. in 
  s#set_transform t;
  assert_equal s#transform t

let test_intersect_scaled_sphere _ =
  let r ={origin=(point 0. 0. (-5.)); direction= (vector 0. 0. 1.)} in
  let s = new sphere in
  s#set_transform (scaling 2. 2. 2.);
  let xs = intersect s r in
  assert_equal 2 (List.length xs);
  assert_equal (3.0)  (List.nth xs 0).t;
  assert_equal 7.0 (List.nth xs 1).t

let test_intersect_translated_sphere _ =
  let r ={origin=(point 0. 0. (-5.)); direction= (vector 0. 0. 1.)} in
  let s = new sphere in
  s#set_transform (translation 5. 0. 0.);
  let xs = intersect s r in
  assert_equal 0 (List.length xs)

let suite =
  "RaysTestList" >::: [
    "test_ray_creation_and_querying" >:: test_ray_creation_and_querying;
    "test_ray_position" >:: test_ray_position;
    "test_ray_intersects_sphere_twice" >:: test_ray_intersects_sphere_twice;
    "test_ray_intersects_sphere_tangent" >:: test_ray_intersects_sphere_tangent;
    "test_ray_misses" >:: test_ray_misses;
    "test_ray_origin_inside_sphere" >:: test_ray_origin_inside_sphere;
    "test_ray_in_front_of_sphere" >:: test_ray_in_front_of_sphere;
    "test_xs_encapsulates_t_and_object" >:: test_xs_encapsulates_t_and_object;
    "test_aggregating_intersections" >:: test_aggregating_intersections;
    "test_hit_all_pos" >:: test_hit_all_pos;
    "test_hit_some_neg" >:: test_hit_some_neg;
    "test_hit_all_neg" >:: test_hit_all_neg;
    "test_hit_many" >:: test_hit_many;
    "test_transform_ray" >:: test_transform_ray;
    "test_scale_ray" >:: test_scale_ray;
    "test_sphere_default_trans" >:: test_sphere_default_trans;
    "test_change_sphere_trans" >:: test_change_sphere_trans;
    "test_intersect_scaled_sphere" >:: test_intersect_scaled_sphere;
    "test_intersect_translated_sphere" >:: test_intersect_translated_sphere;

  ]

let () =
  run_test_tt_main suite