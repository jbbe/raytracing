open Raytracing.Camera
open Raytracing.Matrices
open Raytracing.Shapes
open Raytracing.Transformations
open Raytracing.Tuple
open Raytracing.Color
open Raytracing.Lights
open Raytracing.World

let width = 1000
let height = 500


let color = {r=0.; g=1.;b=0.}
let black = {r=0.; g=0.;b=0.}

let c = new camera width height (pi /. 3.)
let w = new world 

let floor = new shape Plane
let ceiling = new shape Plane
(* let left_wall = new sphere
let right_wall = new sphere *)
let middle = new shape Sphere 
let right = new shape Sphere 
let left = new shape Sphere 
let other = new shape Sphere 
(* let m = default_material () *)
let get_random_color _ =
  {r=Random.float 0.2; g=Random.float 0.6; b=Random.float 0.8;}

  let get_random_color_b _ =
  {r=Random.float 1.; g=Random.float 0.3; b=Random.float 0.6;}
(* m.color <- {r=1.; g=0.2; b=1.}; *)
let floor_mat = {
        ambient=0.1; 
        diffuse=0.9;
        specular=1.;
        shininess=320.;
        refractive_idx=1.3;
        transparency=0.5;
        reflective=0.9;
        pattern=new pattern Gradient [(get_random_color ()); white] }



let light_pos = point (-10.) (-10.) (-10.)
let light_color = {r=1.; g=1.; b=1.}
let _light = point_light light_pos light_color



(* let scale = scaling 1. 0. 0. *)
(* let shift = translation 5. 5. 5. *)
let shear = shearing 0. 1. 0.25 0. 0. 0.5
let right_wall_trans =  matrix_mult (matrix_mult 
(matrix_mult (translation 0. 0. 5.) (rotation_y (pi /. (4.)))) (rotation_x (pi /. 2.))) (scaling 10. 0.01 10.)
let left_wall_trans = matrix_mult (matrix_mult 
      (matrix_mult (translation 0. 0. 5.) (rotation_y (pi /. (-4.)))) (rotation_x (pi /. 2.))) (scaling 10. 0.01 10.)
(* let trans = matrix_mult scale shear *)
let right_transform =  (matrix_mult (translation (-1.5) 0.33 (-0.75)) (scaling 0.3 0.3 0.5))

let main _ =
  Random.init 99;
  let middle_mat = {
        ambient=0.1; 
        diffuse=0.7;
        specular=0.3;
        shininess=2.;
        reflective=1.;
        refractive_idx=3.2;
        transparency=0.7;
        pattern=new pattern Checkers [(get_random_color ()); (get_random_color_b ())]} in
  let right_mat = {
        ambient=0.1; 
        diffuse=0.7;
        specular=0.3;
        shininess=20.;
        reflective=0.6;
        refractive_idx=0.3;
        transparency=0.5;
        pattern=new pattern Ring [(get_random_color_b ()); (get_random_color ())] } in
  (* let left_mat = {
        ambient=0.1; 
        diffuse=0.7;
        specular=0.3;
        shininess=200.;

        pattern=new pattern Checkers [(get_random_color_b ()); (get_random_color_b ())]} in *)
  floor_mat.pattern#set_transform (matrix_mult shear (scaling 20. 5. 20.));
  (* middle_mat.pattern#set_transform (matrix_mult (shearing 0.2 0. 3. 0.1 0. 0.2) (scaling 0.01 0.03 0.5)); *)
  (* middle_mat.pattern#set_transform scali *)
  middle_mat.pattern#set_transform (scaling 0.2 0.2 0.2);
  (* print_matrix middle_mat.pattern#transform; *)
  (* floor#set_transform (scaling 10. 0.01 10.); *)
  ceiling#set_transform (translation 0. 5. 2.);
  ceiling#set_material (right_mat);
  floor#set_material floor_mat;
  (* left_wall#set_transform left_wall_trans;
  left_wall#set_material floor_mat;
  right_wall#set_transform right_wall_trans;
  right_wall#set_material floor_mat; *)
  middle#set_transform (matrix_mult (translation (-0.5) 1.5 0.5) (scaling 1.3 1.3 1.3));
  middle#set_material middle_mat;
  right#set_material right_mat;
  right#set_transform (matrix_mult (translation 1.5 0.5 (-0.5)) (scaling 0.5 0.5 0.5));
  right#set_transform (matrix_mult (translation 1. 0.25 (-0.5)) shear);
  (* left#set_material left_mat; *)
  (* left#set_transform  (matrix_mult (translation (-1.5) 0.33 (-0.75)) (scaling 0.3 0.3 0.3)); *)
  other#set_transform right_transform;
  other#set_material right_mat;
  w#add_light (point_light (point (-10.) 10. (-10.)) white);
  w#add_object floor;
  let g = glass_sphere () in
  w#add_object g;
  (* w#add_object left_wall;
  w#add_object right_wall; *)
  w#add_object middle;
  (* w#add_object right; *)
  (* w#add_object left; *)
  w#add_object other;
  c#set_transform (view_transform (point 0. 1.5 (-5.)) (point 0. 1. 0.) (vector 0. 1. (0.)));
  let img = c#render w in
  let filename = Printf.sprintf "creations/scene  %d .ppm" 87 in
  img#to_file filename;
  ()



let _ = main ()
