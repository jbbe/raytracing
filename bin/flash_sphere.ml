open Raytracing.Canvas
open Raytracing.Rays
open Raytracing.Shapes
open Raytracing.Transformations
open Raytracing.Tuple
open Raytracing.Color
open Raytracing.Lights

let width = 800
let height = 800

let wall_size = 20.
let pixel_size = wall_size /. (float_of_int width)
let half = wall_size /. 2.
let wall_z = 20.
let origin = {x=0.; y=(0.); z=(-5.); w=1.;}

let color = {r=0.; g=1.;b=0.}
let black = {r=0.; g=0.;b=0.}

let c = new canvas width height


let s = new shape Sphere 
(* let m = default_material () *)
let m = {ambient=0.1; 
        diffuse=0.9;
        specular=0.9;
        shininess=20.
        pattern=new pattern Solid [{r=0.8; g=1.; b=0.6}]}
(* m.color <- {r=1.; g=0.2; b=1.}; *)

let light_pos = point (-10.) (-10.) (-10.)
let light_color = {r=1.; g=1.; b=1.}
let _light = point_light light_pos light_color



(* let scale = scaling 1. 0. 0. *)
(* let shift = translation 5. 5. 5. *)
let shear = shearing 0. 1. 0. 0. 0. 1.
(* let trans = matrix_mult scale shear *)

let get_random_color _ =
{r=Random.float 1.; g=0.4; b=Random.float 0.5;}


let rec write_point x y =
  if x = width - 1 
    then 
      (if y = height -1 
        then c#to_file "flashsphere.ppm" 
        else write_point 0 (y + 1) 
      )
    else 
      let world_y = half -. (pixel_size *. (float_of_int y)) in
      let world_x = half -. (pixel_size *. (float_of_int x)) in
      let p = point world_x world_y wall_z in
      let r = {origin=origin; direction=(tuple_sub p origin)} in
      let xs = intersect s r in
      let _hit = hit xs in
      let _point = position r _hit.t in
      let norm = (!(_hit.obj))#normal_at _point in
      let eye = scalar_mult r.direction (-1.) in
      let color_to_write = lighting (s#material) _light _point eye norm in
      (* let color_to_write = if inter = null_x then black else get_random_color () in *)
      c#write_pixel x y color_to_write;
      write_point (x+1) y

let main _ =
  s#set_transform shear;
  s#set_material m;
  write_point 0 0

let _ = main ()
(* for i = 0 to 12 do
  let new_pixel = 
  c#write_pixel (matrix_tuple_mult scale_trans initial);

done; *)


