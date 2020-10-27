open Raytracing.Canvas
open Raytracing.Color

let width = 2000
let height = 2000

let get_depth_color depth =
  if depth = 0 then black else 
  if depth = 1 then {r=1.; g=0.2; b=0.3;} else
  if depth = 2 then {r=0.; g=0.5; b=0.6;} else 
  let r = 0.002 *. (float_of_int depth) in
  let g = sin  (float_of_int depth) in
  let b = tan (float_of_int depth) in
  (* let r =  (float_of_int depth) *. 0.01 in
  let g = cos (float_of_int depth) in
  let b = 0.1 in *)
  {r=r; g=g; b=b;}


let _canvas = new canvas width height

let max_iterations = 1000

let g_min_re = -0.5
let g_max_re = 1.
let g_min_im = 0.5
let g_max_im = g_min_im +. (g_max_re -. g_min_re) *. (float_of_int (height / width))
let g_re_factor = (g_max_re -. g_min_re) /. (float_of_int (width - 1))
let g_im_factor =  (g_max_im -. g_min_im) /. (float_of_int (height - 1))

(* real comes out as y im is x *)

let rec mandelbrot ?(iterations=0) c_re c_im z_re z_im =
  if iterations > max_iterations then iterations
  else 
  let z_re_2 = z_re ** 2. and
  z_im_2 = z_im ** 2. in
  if (z_re_2 +. z_im_2) > 4. then iterations
  else
  let new_z_re = z_re_2 -. z_im_2 +. c_re
  and new_z_im = (2. *. z_re *. z_im) +. c_im in
  mandelbrot ~iterations:(iterations+1) c_re c_im new_z_re new_z_im


let draw_mandelbrot ?(scale=1.) _ =
  let min_re = g_min_re *. scale
  and max_re = g_max_re *. scale
  and min_im = g_min_im *. scale
  and max_im = g_max_im *. scale in
  let re_factor = (max_re -. min_re) /. (float_of_int (width - 1)) in
  let im_factor = (max_im -. min_im) /. (float_of_int (height - 1)) in
  for y = 0 to (width - 1) do
    let c_im = max_im -. ((float_of_int y) *.im_factor) in
    for x = 0 to (height - 1) do
      let c_re = min_re +.  ((float_of_int x) *. re_factor) in
      let z_re = c_re
      and z_im = c_im in
      let depth = mandelbrot c_re c_im z_re z_im in
      let _color = get_depth_color depth in
      _canvas#write_pixel x y _color;
    done;
  done;
  let file_name = "creations/mandelbrot_12_" ^ (string_of_float scale) ^  ".ppm" in
  _canvas#to_file file_name;
  ()


let scales = [1.; 0.1; 0.01; 0.001]

(* let rec zoom scales = 
  match scales with
  | [] -> ()
  | scale::rest -> draw_mandelbrot ~scale:scale (); zoom rest
 *)

let _ =  draw_mandelbrot ~scale:0.4 ()

