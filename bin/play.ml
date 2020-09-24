
open Raytracing.Rays
open Raytracing.Shapes
open Raytracing.Transformations
open Raytracing.Tuple
open Raytracing.Color
open Raytracing.Intersections
open Raytracing.World

let main _ =
  let w = default_world () in
  let s = new shape Plane in
  s#material.reflective <- 0.5;
  s#set_transform (translation 0. (-1.) 0.);
  w#add_object s;
  let deg45 = (sqrt 2.) /. 2. in
  let r = {origin=(point 0. 0. (-3.)); direction=(vector 0. ((-1.) *. deg45) deg45)} in
  let i = {t=(sqrt 2.); obj=(ref s)} in
  (* w#print_world; *)
  let comps = prepare_computations i r in
  (* Printf.printf "\n\ntest reflected color\n"; *)
  let c = w#reflected_color comps in
  Printf.printf "\n\nReflected color calculated ************\n";

  (* print_computations comps; *)
  print_color c;
  ()
  (* assert_bool "reflecting" (color_equal {r=0.19032; g=0.2379; b=0.14274} c) *)

let _ = main ()