open Matrices
open Tuple

let pi = 4.0 *. atan 1.0


let translation (x: float) (y: float) (z: float) : float array array =
  let trans = make_identity () in
  trans.(0).(3) <- x;
  trans.(1).(3) <- y;
  trans.(2).(3) <- z;
  trans

let scaling (x: float) (y: float) (z: float) : float array array =
  let trans = make_identity () in
  trans.(0).(0) <- x;
  trans.(1).(1) <- y;
  trans.(2).(2) <- z;
  trans

let rotation_x r =
  let trans = make_identity () in
  trans.(1).(1) <- (cos r);
  trans.(1).(2) <- ( -1. *. (sin r));
  trans.(2).(1) <- (sin r);
  trans.(2).(2) <- (cos r);
  trans

let rotation_y r =
  let trans = make_identity () in
  trans.(0).(0) <- (cos r);
  trans.(0).(2) <- (sin r);
  trans.(2).(0) <- ( -1. *. (sin r));
  trans.(2).(2) <- (cos r);
  trans

let rotation_z r =
  let trans = make_identity () in
  trans.(0).(0) <- (cos r);
  trans.(0).(1) <- ( -1. *. (sin r));
  trans.(1).(0) <- (sin r);
  trans.(1).(1) <- (cos r);
  trans

let shearing x_y x_z y_x y_z z_x z_y =
  let trans = make_identity () in
  trans.(0).(1) <- x_y;
  trans.(0).(2) <- x_z;
  trans.(1).(0) <- y_x;
  trans.(1).(2) <- y_z;
  trans.(2).(0) <- z_x;
  trans.(2).(1) <- z_y;
  trans

let view_transform (_from : tuple) (_to : tuple) (_up : tuple)  =
  let forward = normalize (tuple_sub _to _from) in 
  let upn = normalize _up in
  let left = cross forward upn in
  let true_up = cross left forward in
  let orientation = Array.make_matrix 4 4 0. in
  orientation.(0).(0) <- left.x;
  orientation.(0).(1) <- left.y;
  orientation.(0).(2) <- left.z;
  orientation.(0).(3) <- 0.;
  orientation.(1).(0) <- true_up.x;
  orientation.(1).(1) <- true_up.y;
  orientation.(1).(2) <- true_up.z;
  orientation.(1).(3) <- 0.;
  orientation.(2).(0) <- ((-1.) *. forward.x);
  orientation.(2).(1) <-  ((-1.) *. forward.y);
  orientation.(2).(2) <-  ((-1.) *. forward.z);
  orientation.(2).(3) <- (0.);
  orientation.(3).(0) <- (0.);
  orientation.(3).(1) <- (0.);
  orientation.(3).(2) <- (0.);
  orientation.(3).(3) <- (1.);  
  matrix_mult orientation (translation ((-1.) *. _from.x) ((-1.) *. _from.y) ((-1.) *. _from.z))