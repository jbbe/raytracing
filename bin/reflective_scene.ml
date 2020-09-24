open Raytracing.Camera
open Raytracing.Shapes
open Raytracing.Transformations
open Raytracing.Tuple
open Raytracing.Color
open Raytracing.Lights
open Raytracing.World

let width =600 
let height = 300


let color = {r=0.; g=1.;b=0.}
let black = {r=0.; g=0.;b=0.}

let main _ =
  Random.init 8;
  let c = new camera width height (pi /. 3.) in
  let w = default_world () in
  let s = new shape Plane in
  s#material.reflective <- 0.5;
  s#set_transform (translation 0. (-1.) 0.);
  w#add_object s;
  c#set_transform (view_transform (point 0. 1.5 (-5.)) (point 0. 1. 0.) (vector 0. 1. (0.)));
  let img = c#render w in
  let filename = Printf.sprintf "creations/test %d .ppm" (Random.int 173) in
  img#to_file filename;
  ()



let _ = main ()
