open Matrices

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