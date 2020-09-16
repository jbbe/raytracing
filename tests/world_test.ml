open OUnit2
open Raytracing.Tuple
open Raytracing.Color
open Raytracing.Matrices
open Raytracing.Rays
open Raytracing.Sphere
open Raytracing.Transformations
open Raytracing.Lights
open Raytracing.World

let create_world _ =
  let w = new world in
  assert_equal [] (w#objects);
  assert_equal [] (w#lights)

let default_world _ =
  let _light = point_light (point (-10.) 10. (-10.)) (white ()) in
  let s1 = new sphere in 
  let m1 = {color={r=0.8; g=1.; b=0.6};
            ambient=0.1; 
            diffuse=0.7;
            specular=0.2;
          shininess=200.} in
  s1#set_material m1;
  let s2 = new sphere in
  let trans = scaling 0.5 0.5 0.5 in
  s2#set_transform trans;
  let w = default_world () in
  assert_equal w#lights [_light];
  (* print_spheres w#objects;
  print_spheres ([s1;s2]); *)
  assert_equal m1 (((List.nth w#objects 0)#material));
  assert_bool "s2 in w" (((List.nth w#objects 1)#transform) =  trans)


let world_ray_intersec _ =
  let w = default_world () in
  let r =  {origin=(point 0. 0. (-5.)); direction=(vector 0. 0. 1.)} in
  print_spheres (w#objects)
  (* let xs = (w#intersect r) in
  assert_equal 4 (List.length xs 4);
  assert_equal (4.0)  (List.nth xs 0).t;
  assert_equal 4.5 (List.nth xs 1).t;
  assert_equal 5.5 (List.nth xs 2).t;
  assert_equal 6.0 (List.nth xs 3).t *)


let suite =
  "WorldList" >::: [
    "create_world" >:: create_world;
    (* "default_world" >:: default_world; *)
    "world_ray_intersec" >:: world_ray_intersec;

  ]

let () =
  run_test_tt_main suite