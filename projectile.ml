open Tuple
open Color
open Canvas

type projectile = {pos : tuple; velocity: tuple}
type environment = {gravity: tuple; wind: tuple}
let height = 200
let width = 300
let origin = {x=0.; y=0.; z=0.; w=1.}
let e = {gravity={x=0.; y=(-0.1); z=0.; w=0.}; wind={x=(-0.01); y=0.; z=0.; w=0.}}
let start_point = {x=0.;y=1.;z=0.;w=1.}
let velo = scalar_mult (normalize {x=3.; y=5.; z=0.; w=0.}) 5. ;;
let p = {pos=start_point; velocity=velo}
let c = new canvas width height
let path_color = {r=1.; g=0.;b=0.}
(* let fmt = format_of_string "Projectile at %f %f %f\n" *)


let rec tick (env: environment) (proj: projectile) =
  if proj.pos.y <= 0. then c#to_file "proj.ppm"
  else
    (* let _ = Printf.printf fmt proj.pos.x proj.pos.y proj.pos.z in *)
    let int_y = int_of_float proj.pos.y in
    let y_to_write = if int_y >= 0 && int_y <= (height - 1) then ((height-1) - int_y) else 19 in
    let int_x = int_of_float proj.pos.x in
    let x_to_write = if int_x >= 0 && int_x <= (width - 1) then (int_x) else 0 in
    (* Printf.printf "%d %d\n" int_y y_to_write; *)
    c#write_pixel x_to_write y_to_write path_color;
    let new_pos = tuple_add proj.pos proj.velocity in
    let new_velocity = tuple_add env.wind  (tuple_add proj.velocity env.gravity) in 
      tick env {pos=new_pos; velocity=new_velocity}

let _ = tick e p