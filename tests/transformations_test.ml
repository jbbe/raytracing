open OUnit2
open Tuple
open Matrices
open Transformations


let multiply_by_trans_matrix _ =
  let trans = translation 5. (-3.) 2. in
  let p = {x=(-3.); y=4.; z=5.; w=1.} in
  let res = matrix_tuple_mult trans p in
  let correct =  {x=(2.); y=1.; z=7.; w=1.} in
  assert_equal correct res

let multiply_by_inverse_of_trans_matrix _ =
  let trans = translation 5. (-3.) 2. in
  let inv = inverse trans in
  let p = {x=(-3.); y=4.; z=5.; w=1.} in
  let res = matrix_tuple_mult inv p in
  let correct =  {x=(-8.); y=7.; z=3.; w=1.} in
  assert_equal correct res

let translation_does_not_affect_vector _ =
  let trans = translation 5. (-3.) 2. in
  let inv = inverse trans in
  let v = {x=(-3.); y=4.; z=5.; w=0.} in
  let res = matrix_tuple_mult inv v in
  assert_equal v res

let test_scale_point _ =
  let trans = scaling 2. 3. 4. in
  let p = {x=(-4.); y=(6.); z=8.; w=1.} in
  let correct = {x=(-8.); y=(18.); z=32.; w=1.} in
  let res = matrix_tuple_mult trans p in
  (* let () = trans |> Array.iter (Array.iter print_float_newline) in *)
    (* print_float res.x;
  print_float res.y;
  print_float res.z;
  print_float res.w;  
  print_float correct.x;
  print_float correct.y;
  print_float correct.z;
  print_float correct.w; *)
  assert_equal correct res

let test_scale_vec _ =
  let trans = scaling 2. 3. 4. in
  let v = {x=(-4.); y=(6.); z=8.; w=0.} in
  let correct = {x=(-8.); y=(18.); z=32.; w=0.} in
  let res = matrix_tuple_mult trans v in
  assert_equal correct res

let test_scale_inverse_vec _ =
  let trans = scaling 2. 3. 4. in
  let inv = inverse trans in
  let v = {x=(-4.); y=(6.); z=8.; w=0.} in
  let correct = {x=(-2.); y=(2.); z=2.; w=0.} in
  let res = matrix_tuple_mult inv v in
  assert_equal correct res

let test_reflection _ =
  let trans = scaling (-1.) 1. 1. in
  let p = {x=(2.); y=(3.); z=4.; w=1.} in
  let correct = {x=(-2.); y=(3.); z=4.; w=1.} in
  let res = matrix_tuple_mult trans p in
  (* print_float res.x;
  print_float res.y;
  print_float res.z; *)
  assert_equal correct res

let test_x_rotation _ =
  let p = {x=(0.); y=(1.); z=0.; w=1.} in
  let half_quarter = rotation_x (pi /. 4.) in
  let full_quarter = rotation_x (pi /. 2.) in
  let inv_quarter = inverse half_quarter in
  let half_point = {x=(0.); y=((sqrt 2.) /. 2.); z=((sqrt 2.) /. 2.); w=1.} in
  let full_point = {x=(0.); y=(0.); z=1.; w=1.} in
  let inv_point =  {x=(0.); y=((sqrt 2.) /. 2.); z=(-1. *. (sqrt 2.) /. 2.); w=1.} in
  let half_res = matrix_tuple_mult half_quarter p in
  let full_res = matrix_tuple_mult full_quarter p in
  let inv_res = matrix_tuple_mult inv_quarter p in
  assert_bool "half quarter rotation x" (tuple_equal half_res half_point);
  assert_bool "full quarter rotation x" (tuple_equal full_res full_point);
  assert_bool "inv quarter rotation x" (tuple_equal inv_res inv_point)

let test_y_rotation _ =
  let p = {x=(0.); y=(0.); z=1.; w=1.} in
  let half_quarter = rotation_y (pi /. 4.) in
  let full_quarter = rotation_y (pi /. 2.) in
  let half_point = {x=((sqrt 2.) /. 2.); y=0.; z=((sqrt 2.) /. 2.); w=1.} in
  let full_point = {x=(1.); y=(0.); z=0.; w=1.} in
  let half_res = matrix_tuple_mult half_quarter p in
  let full_res = matrix_tuple_mult full_quarter p in
  assert_bool "half quarter rotation y" (tuple_equal half_res half_point);
  assert_bool "full quarter rotation y" (tuple_equal full_res full_point)

let test_z_rotation _ =
  let p = {x=(0.); y=(1.); z=0.; w=1.} in
  let half_quarter = rotation_z (pi /. 4.) in
  let full_quarter = rotation_z (pi /. 2.) in
  let half_point = {x=(-1. *. (sqrt 2.) /. 2.); y=((sqrt 2.) /. 2.); z=0.; w=1.} in
  let full_point = {x=(-1.); y=(0.); z=0.; w=1.} in
  let half_res = matrix_tuple_mult half_quarter p in
  let full_res = matrix_tuple_mult full_quarter p in
  assert_bool "half quarter rotation z" (tuple_equal half_res half_point);
  assert_bool "full quarter rotation z" (tuple_equal full_res full_point)

let test_shearing_x_to_y _ =
  let trans = shearing 1. 0. 0. 0. 0. 0. in
  let p = {x=(2.); y=(3.); z=4.; w=1.} in
  let res = matrix_tuple_mult trans p in
  let correct = {x=(5.); y=(3.); z=4.; w=1.} in
  assert_bool "shearing x to y" (tuple_equal correct res)

let test_shearing_x_to_z _ =
  let trans = shearing 0. 1. 0. 0. 0. 0. in
  let p = {x=(2.); y=(3.); z=4.; w=1.} in
  let res = matrix_tuple_mult trans p in
  let correct = {x=(6.); y=(3.); z=4.; w=1.} in
  assert_bool "shearing x to z" (tuple_equal correct res)

let test_shearing_y_to_x _ =
  let trans = shearing 0. 0. 1. 0. 0. 0. in
  let p = {x=(2.); y=(3.); z=4.; w=1.} in
  let res = matrix_tuple_mult trans p in
  let correct = {x=(2.); y=(5.); z=4.; w=1.} in
  assert_bool "shearing y to x" (tuple_equal correct res)

let test_shearing_y_to_z _ =
  let trans = shearing 0. 0. 0. 1. 0. 0. in
  let p = {x=(2.); y=(3.); z=4.; w=1.} in
  let res = matrix_tuple_mult trans p in
  let correct = {x=(2.); y=(7.); z=4.; w=1.} in
  assert_bool "shearing y to z" (tuple_equal correct res)

let test_shearing_z_to_x _ =
  let trans = shearing 0. 0. 0. 0. 1. 0. in
  let p = {x=(2.); y=(3.); z=4.; w=1.} in
  let res = matrix_tuple_mult trans p in
  let correct = {x=(2.); y=(3.); z=6.; w=1.} in
  assert_bool "shearing z to x" (tuple_equal correct res)

let test_shearing_z_to_y _ =
  let trans = shearing 0. 0. 0. 0. 0. 1. in
  let p = {x=(2.); y=(3.); z=4.; w=1.} in
  let res = matrix_tuple_mult trans p in
  let correct = {x=(2.); y=(3.); z=7.; w=1.} in
  assert_bool "shearing z to y" (tuple_equal correct res)

let test_shearing_z_to_x _ =
  let trans = shearing 0. 0. 0. 0. 1. 0. in
  let p = {x=(2.); y=(3.); z=4.; w=1.} in
  let res = matrix_tuple_mult trans p in
  let correct = {x=(2.); y=(3.); z=6.; w=1.} in
  assert_bool "shearing z to y" (tuple_equal correct res)
  
let test_trans_in_sequence _ =
  let p = {x=(1.); y=(0.); z=1.; w=1.} in
  let a = rotation_x (pi /. 2.) in
  let b = scaling 5. 5. 5. in
  let c = translation 10. 5. 7. in
  let p2 = matrix_tuple_mult a p in
  let p2_correct = {x=(1.); y=(-1.); z=0.; w=1.} in
  let p3 = matrix_tuple_mult b p2 in
  let p3_correct = {x=(5.); y=(-5.); z=0.; w=1.} in
  let p4 = matrix_tuple_mult c p3 in
  let p4_correct = {x=(15.); y=(0.); z=7.; w=1.} in
  assert_bool "rotation first" (tuple_equal p2_correct p2);
  assert_bool "then scaling" (tuple_equal p3_correct p3);
  assert_bool "then translation" (tuple_equal p4_correct p4)

let test_trans_chaining _ =
  let p = {x=(1.); y=(0.); z=1.; w=1.} in
  let a = rotation_x (pi /. 2.) in
  let b = scaling 5. 5. 5. in
  let c = translation 10. 5. 7. in 
  let chained_trans = matrix_mult (matrix_mult c b) a in
  let res = matrix_tuple_mult chained_trans p in
  let correct = {x=(15.); y=(0.); z=7.; w=1.} in
  assert_bool "chained transation" (tuple_equal res correct)


let suite =
  "TranslationTestList" >::: [
    "multiply_by_trans_matrix" >:: multiply_by_trans_matrix;
    "multiply_by_inverse_of_trans_matrix" >:: multiply_by_inverse_of_trans_matrix;
    "translation_does_not_affect_vector" >:: translation_does_not_affect_vector;
    "test_scale_point" >:: test_scale_point;
    "test_scale_vec" >:: test_scale_vec;
    "test_scale_inverse_vec" >:: test_scale_inverse_vec;
    "test_reflection" >:: test_reflection;
    "test_x_rotation" >:: test_x_rotation;
    "test_y_rotation" >:: test_y_rotation;
    "test_z_rotation" >:: test_z_rotation;
    "test_shearing_x_to_y" >:: test_shearing_x_to_y;
    "test_shearing_x_to_z" >:: test_shearing_x_to_z;
    "test_shearing_y_to_x" >:: test_shearing_y_to_x;
    "test_shearing_y_to_z" >:: test_shearing_y_to_z;
    "test_shearing_z_to_x" >:: test_shearing_z_to_x;
    "test_shearing_z_to_y" >:: test_shearing_z_to_y;
    "test_trans_in_sequence" >:: test_trans_in_sequence;
    "test_trans_chaining" >:: test_trans_chaining;

  ]

let () =
  run_test_tt_main suite