open Canvas
open Rays
open Sphere
open Canvas
open Matrices
open Transformations
open Tuple
open Color

let width = 500
let height = 500

let wall_size = 20.
let pixel_size = wall_size /. (float_of_int width)
let half = wall_size /. 2.
let wall_z = 10.
let origin = {x=0.; y=0.; z=(-5.); w=1.;}

let color = {r=0.; g=1.;b=0.}
let black = {r=0.; g=0.;b=0.}

let c = new canvas width height


let s = new sphere

let scale = scaling 2. 1. 0.
(* let shift = translation 5. 5. 5. *)
let shear = shearing 0. 0. 2. 0. 0. 1.
let trans = matrix_mult scale shear

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
      let inter = hit xs in
      let color_to_write = if inter = null_x then black else get_random_color () in
      c#write_pixel x y color_to_write;
      write_point (x+1) y

let main _ =
  s#set_transform shear;
  write_point 0 0

let _ = main ()
(* for i = 0 to 12 do
  let new_pixel = 
  c#write_pixel (matrix_tuple_mult scale_trans initial);

done; *)


