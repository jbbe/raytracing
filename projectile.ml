open Tuple

type projectile = {pos : tuple; velocity: tuple}
type environment = {gravity: tuple; wind: tuple}

let origin = {x=0.; y=0.; z=0.; w=1.}
let e = {gravity={x=0.; y=(-0.1); z=0.; w=0.}; wind={x=(-0.01); y=0.; z=0.; w=0.}}
let velo = normalize {x=3.; y=10.; z=0.; w=0.};;
let p = {pos={x=0.;y=1.;z=0.;w=1.}; velocity=velo}

let fmt = format_of_string "Projectile at %f %f %f\n"
(* let print_status (proj: projectile) : unit =
   *)

let rec tick (env: environment) (proj: projectile) =
  if proj.pos.y <= 0. then Printf.printf "Crashed!"
  else
    let _ = Printf.printf fmt proj.pos.x proj.pos.y proj.pos.z in
    let new_pos = tuple_add proj.pos proj.velocity in
    let new_velocity = tuple_add env.wind  (tuple_add proj.velocity env.gravity) in 
      tick env {pos=new_pos; velocity=new_velocity}

let _ = tick e p