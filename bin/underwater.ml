open Raytracing.Camera
open Raytracing.Shapes
open Raytracing.Transformations
open Raytracing.Tuple
open Raytracing.Color
open Raytracing.Lights
open Raytracing.Matrices
open Raytracing.World

let width = 800 
let height = 400


let main _ =
  Random.init 8;
  let c = new camera width height (pi /. 2.) in
  let w = new world  in
  let surface = new shape Plane in
  surface#set_material (default_material ());
  surface#material.reflective <- 0.3;
  surface#material.refractive_idx <- 1.5;
  surface#material.transparency <- 0.9;
  let floor = new shape Plane in
  let s2 = glass_sphere () in
  s2#material.reflective <- 0.5;
  s2#set_transform (translation 0. (2.) (-4.));
  let s2_pat = new pattern Checkers [{r=0.4; g=0.6; b=0.3}; {r=0.8; g=0.3; b=0.2}] in
  s2#material.pattern <- s2_pat ;
  let s = glass_sphere () in
  let floor_pat = new pattern Ring [{r=0.1; g=0.8; b=0.9}; {r=0.2; g=0.7; b=0.2}] in
  floor_pat#set_transform (matrix_mult (scaling 10. 13. 0.3) (translation 0. 0. (-5.)));
  floor#material.pattern <- floor_pat;
  floor#set_transform (translation 0. (-2.) 0.);
  s#material.reflective <- 0.5;
  s#set_transform (translation 0. (-1.) (-3.));
  let s_pat = new pattern Checkers [{r=0.4; g=0.6; b=0.3}; {r=0.8; g=0.3; b=0.2}] in
  s#material.pattern <- s_pat ;
  (* let a = List.nth w#objects 0 in
  a#material.ambient  <- 1.;
  a#material.reflective <- 0.8;
  a#material.refractive_idx <- 0.8;
  a#material.transparency  <- 0.8;
  a#material.pattern <- new pattern Gradient [{r=0.4; g=0.6; b=0.3}; {r=0.8; g=0.3; b=0.2}];
  let b = List.nth w#objects 1 in
  b#material.transparency  <- 1.;
  b#material.refractive_idx <- 1.5; *)
  w#add_object surface;
  w#add_object s2;
  w#add_object s;
  w#add_object floor;
  c#set_transform (view_transform (point 0. 1.5 (-10.)) (point 0. 1. 0.) (vector 0. 1. (0.)));
  let img = c#render w in
  let filename = Printf.sprintf "creations/creation %d .ppm"  164 in
  img#to_file filename;
  ()



let _ = main ()
