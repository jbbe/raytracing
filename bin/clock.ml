open Canvas
open Matrices
open Transformations
open Tuple
open Color

let inital = {x=0.; y=0.; z=1.; w=0.;}

let width = 2000
let height = 2000

let n = 400


let radius = (float_of_int height) /. 2.
(* let origin = {x=radius; y=radius; z=0.; w=0.;} *)
let origin = {x=0.; y=0.; z=0.; w=0.;}
let center_coord = {x=radius; y=0.; z=radius; w=1.;}
let twelve_o_clock = {x=0.; y=0.; z=1.; w=0.;}
(* let color = {r=1.; g=0.5;b=0.6} *)
let color = {r=0.; g=1.;b=0.}
(* let c = new canvas height height *)
let c = new canvas width height

(* let trans_scale = scaling radius radius 0. *)
let scale_factor = (3. /. 8.) *. radius

let rotation_factor = pi /. 40.

let rotate hour = 
  let r = rotation_y ((float_of_int hour) *. rotation_factor) in
  matrix_tuple_mult r twelve_o_clock
  
(* let rans_rotate =  *)

(* let pix = matrix_tuple_mult trans_scale (tuple_add inital  origin) *)

let y_of_pixel p : int = 
  height - (int_of_float p.z)

let x_of_pixel p : int =
  int_of_float p.x


let pixel_of_tuple t hour =
  (* let shifted = tuple_add t center_coord in *)
  (* let translation = translation radius 0. radius in *)
  let scale_factor = ((hour ) /. ((float_of_int n))) *. radius in
  let scale = scaling scale_factor 0. scale_factor in
  (* let trans = matrix_mult translation scale in *)
  let scaled = matrix_tuple_mult scale t in
  tuple_add scaled center_coord

(* let add_hour (last_hour) =
  let  *)
(* let new_x = 
let new_y = int_of_float new_pixel.y *)


let get_random_color _ =
{r=Random.float 1.; g=Random.float 1.; b=Random.float 1.;}

let rec write_point hour _ =
  if hour = n then c#to_file "clock.ppm"
  else 
    let position = rotate hour in
    print_tuple position;
    let pix = pixel_of_tuple position (float_of_int hour) in
    print_tuple pix;
    c#write_pixel (x_of_pixel pix) (y_of_pixel pix) (get_random_color ());
    write_point (hour + 1)
    ()

let main _ =
  write_point 0
  (* let x = x_of_pixel pix in
  let y = y_of_pixel pix in
  print_tuple pix;
  write_square x y color c; *)
  
  ()

let _ = main ()
(* for i = 0 to 12 do
  let new_pixel = 
  c#write_pixel (matrix_tuple_mult scale_trans initial);

done; *)


