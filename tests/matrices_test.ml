open OUnit2
open Matrices
open Tuple

let test_4_by_4_creation _ =
  let m = Array.make_matrix 4 4 0. in
  m.(0).(0) <- 1.;
  m.(0).(1) <- 2.;
  m.(0).(2) <- 3.;
  m.(0).(3) <- 4.;
  m.(1).(0) <- 5.5;
  m.(1).(1) <- 6.5;
  m.(1).(2) <- 7.5;
  m.(1).(3) <- 8.5;
  m.(2).(0) <- 9.;
  m.(2).(1) <- 10.;
  m.(2).(1) <- 11.;
  m.(2).(2) <- 12.;
  m.(2).(3) <- 13.5;
  m.(3).(1) <- 14.5;
  m.(3).(2) <- 15.5;
  m.(3).(3) <- 16.5;
  assert_equal 1. m.(0).(0);
  assert_equal 4. m.(0).(3);
  assert_equal 5.5 m.(1).(0);
  assert_equal 7.5 m.(1).(2);
  assert_equal 12. m.(2).(2)

let test_2_by_2_creation _ =
  let m = Array.make_matrix 2 2 0. in
  m.(0).(0) <- (-3.);
  m.(0).(1) <- 5.;
  m.(1).(0) <- 1.;
  m.(1).(1) <- (-2.);

  assert_equal (-3.) m.(0).(0);
  assert_equal 5. m.(0).(1);
  assert_equal 1. m.(1).(0);
  assert_equal (-2.) m.(1).(1)

let test_3_by_3_creation _ =
  let m = Array.make_matrix 3 3 0. in
  m.(0).(0) <- (-3.);
  m.(0).(1) <- 5.;
  m.(0).(2) <- 0.;
  m.(1).(0) <- (1.);
  m.(1).(1) <- (-2.);
  m.(1).(2) <- (-7.);
  m.(2).(1) <- 1.;
  m.(2).(2) <- 1.;
  assert_equal (-3.) m.(0).(0);
  assert_equal (-2.) m.(1).(1);
  assert_equal 1. m.(2).(2);
  assert_equal 0. m.(2).(0)

let test_matrix_equality _ =
  let m1 = Array.make_matrix 4 4 0. in
  m1.(0).(0) <- 1.;
  m1.(0).(1) <- 2.;
  m1.(0).(2) <- 3.;
  m1.(0).(3) <- 4.;
  m1.(1).(0) <- 5.5;
  m1.(1).(1) <- 6.5;
  m1.(1).(2) <- 7.5;
  m1.(1).(3) <- 8.5;
  m1.(2).(0) <- 9.;
  m1.(2).(1) <- 10.;
  m1.(2).(1) <- 11.;
  m1.(2).(2) <- 12.;
  m1.(2).(3) <- 13.5;
  m1.(3).(1) <- 14.5;
  m1.(3).(2) <- 15.5;
  m1.(3).(3) <- 16.5;
  let m2 = Array.make_matrix 4 4 0. in
  m2.(0).(0) <- 1.;
  m2.(0).(1) <- 2.;
  m2.(0).(2) <- 3.;
  m2.(0).(3) <- 4.;
  m2.(1).(0) <- 5.5;
  m2.(1).(1) <- 6.5;
  m2.(1).(2) <- 7.5;
  m2.(1).(3) <- 8.5;
  m2.(2).(0) <- 9.;
  m2.(2).(1) <- 10.;
  m2.(2).(1) <- 11.;
  m2.(2).(2) <- 12.;
  m2.(2).(3) <- 13.5;
  m2.(3).(1) <- 14.5;
  m2.(3).(2) <- 15.5;
  m2.(3).(3) <- 16.5;
  assert_bool "equal matrices" (m1 = m2)


let test_matrix_inequality _ =
  let m1 = Array.make_matrix 4 4 0. in
  m1.(0).(0) <- 1.;
  m1.(0).(1) <- 2.;
  m1.(0).(2) <- 3.;
  m1.(0).(3) <- 4.;
  m1.(1).(0) <- 5.5;
  m1.(1).(1) <- 6.5;
  m1.(1).(2) <- 7.5;
  m1.(1).(3) <- 8.5;
  m1.(2).(0) <- 9.;
  m1.(2).(1) <- 10.;
  m1.(2).(1) <- 11.;
  m1.(2).(2) <- 12.;
  m1.(2).(3) <- 13.5;
  m1.(3).(1) <- 14.5;
  m1.(3).(2) <- 15.5;
  m1.(3).(3) <- 16.5;
  let m2 = Array.make_matrix 4 4 0. in
  m2.(0).(0) <- 1.;
  m2.(0).(1) <- 2.;
  m2.(0).(2) <- 3.;
  m2.(0).(3) <- 4.;
  m2.(1).(0) <- 5.5;
  m2.(1).(1) <- 6.5;
  m2.(1).(2) <- 7.5;
  m2.(1).(3) <- 8.5;
  m2.(2).(0) <- 2.;
  m2.(2).(1) <- 10.;
  m2.(2).(1) <- 11.;
  m2.(2).(2) <- 12.;
  m2.(2).(3) <- 13.5;
  m2.(3).(1) <- 14.5;
  m2.(3).(2) <- 15.5;
  m2.(3).(3) <- 1.;
  assert_bool "unequal matrices" (not (m1 = m2))

let test_matrix_mult _ =
  let m1 = Array.make_matrix 4 4 0. in
  m1.(0).(0) <- 1.;
  m1.(0).(1) <- 2.;
  m1.(0).(2) <- 3.;
  m1.(0).(3) <- 4.;
  m1.(1).(0) <- 5.;
  m1.(1).(1) <- 6.;
  m1.(1).(2) <- 7.;
  m1.(1).(3) <- 8.;
  m1.(2).(0) <- 9.;
  m1.(2).(1) <- 8.;
  m1.(2).(2) <- 7.;
  m1.(2).(3) <- 6.;
  m1.(3).(0) <- 5.;
  m1.(3).(1) <- 4.;
  m1.(3).(2) <- 3.;
  m1.(3).(3) <- 2.;
  let m2 = Array.make_matrix 4 4 0. in
  m2.(0).(0) <- (-2.);
  m2.(0).(1) <- 1.;
  m2.(0).(2) <- 2.;
  m2.(0).(3) <- 3.;
  m2.(1).(0) <- 3.;
  m2.(1).(1) <- 2.;
  m2.(1).(2) <- 1.;
  m2.(1).(3) <- -1.;
  m2.(2).(0) <- 4.;
  m2.(2).(1) <- 3.;
  m2.(2).(2) <- 6.;
  m2.(2).(3) <- 5.;
  m2.(3).(0) <- 1.;
  m2.(3).(1) <- 2.;
  m2.(3).(2) <- 7.;
  m2.(3).(3) <- 8.;
  let m3 = Array.make_matrix 4 4 0. in
  m3.(0).(0) <- 20.;
  m3.(0).(1) <- 22.;
  m3.(0).(2) <- 50.;
  m3.(0).(3) <- 48.;
  m3.(1).(0) <- 44.;
  m3.(1).(1) <- 54.;
  m3.(1).(2) <- 114.;
  m3.(1).(3) <- 108.;
  m3.(2).(0) <- 40.;
  m3.(2).(1) <- 58.;
  m3.(2).(2) <- 110.;
  m3.(2).(3) <- 102.;
  m3.(3).(0) <- 16.;
  m3.(3).(1) <- 26.;
  m3.(3).(2) <- 46.;
  m3.(3).(3) <- 42.; 
  let res = matrix_mult m1 m2 in
  assert_bool "matrix mult" (m3 = res)


let test_tuple_matrix_mult _ =
  let m1 = Array.make_matrix 4 4 0. in
  m1.(0).(0) <- 1.;
  m1.(0).(1) <- 2.;
  m1.(0).(2) <- 3.;
  m1.(0).(3) <- 4.;
  m1.(1).(0) <- 2.;
  m1.(1).(1) <- 4.;
  m1.(1).(2) <- 4.;
  m1.(1).(3) <- 2.;
  m1.(2).(0) <- 8.;
  m1.(2).(1) <- 6.;
  m1.(2).(2) <- 4.;
  m1.(2).(3) <- 1.;
  m1.(3).(0) <- 0.;
  m1.(3).(1) <- 0.;
  m1.(3).(2) <- 0.;
  m1.(3).(3) <- 1.;
  let b = {x=1.; y=2.; z=3.; w=1.} in
  let correct = {x=18.; y=24.; z=33.; w=1.} in
  let prod = matrix_tuple_mult m1 b in
  assert_bool "test a" (tuple_equal prod correct)


let test_identity_matrix _ =
  let m1 = Array.make_matrix 4 4 0. in
  m1.(0).(0) <- 1.;
  m1.(0).(1) <- 2.;
  m1.(0).(2) <- 3.;
  m1.(0).(3) <- 4.;
  m1.(1).(0) <- 2.;
  m1.(1).(1) <- 4.;
  m1.(1).(2) <- 4.;
  m1.(1).(3) <- 2.;
  m1.(2).(0) <- 8.;
  m1.(2).(1) <- 6.;
  m1.(2).(2) <- 4.;
  m1.(2).(3) <- 1.;
  m1.(3).(0) <- 0.;
  m1.(3).(1) <- 0.;
  m1.(3).(2) <- 0.;
  m1.(3).(3) <- 1.;
  let m2 = matrix_mult m1 identity_matrix in
  (* let () = identity_matrix |> Array.iter (Array.iter print_float) in *)
  (* let () = m2 |> Array.iter (Array.iter print_float) in *)
  assert_bool "Identity mult test" (m1 = m2)


let test_transpose _ =
  let m1 = Array.make_matrix 4 4 0. in
  m1.(0).(0) <- 0.;
  m1.(0).(1) <- 9.;
  m1.(0).(2) <- 3.;
  m1.(0).(3) <- 0.;
  m1.(1).(0) <- 9.;
  m1.(1).(1) <- 8.;
  m1.(1).(2) <- 0.;
  m1.(1).(3) <- 8.;
  m1.(2).(0) <- 1.;
  m1.(2).(1) <- 8.;
  m1.(2).(2) <- 5.;
  m1.(2).(3) <- 3.;
  m1.(3).(0) <- 0.;
  m1.(3).(1) <- 0.;
  m1.(3).(2) <- 5.;
  m1.(3).(3) <- 8.;
  let m_correct = Array.make_matrix 4 4 0. in
  m_correct.(0).(0) <- 0.;
  m_correct.(0).(1) <- 9.;
  m_correct.(0).(2) <- 1.;
  m_correct.(0).(3) <- 0.;
  m_correct.(1).(0) <- 9.;
  m_correct.(1).(1) <- 8.;
  m_correct.(1).(2) <- 8.;
  m_correct.(1).(3) <- 0.;
  m_correct.(2).(0) <- 3.;
  m_correct.(2).(1) <- 0.;
  m_correct.(2).(2) <- 5.;
  m_correct.(2).(3) <- 5.;
  m_correct.(3).(0) <- 0.;
  m_correct.(3).(1) <- 8.;
  m_correct.(3).(2) <- 3.;
  m_correct.(3).(3) <- 8.;
  let m_trans = transpose m1 in
  assert_bool "trans test" (m_correct = m_trans) 

let trans_identity _ =
  let trans = transpose identity_matrix in
  assert_bool "trans identity is trans identity" (trans = identity_matrix) 

let test_determination _ =
  let m = Array.make_matrix 2 2 0. in
  m.(0).(0) <- 1.;
  m.(0).(1) <- 5.;
  m.(1).(0) <- (-3.);
  m.(1).(1) <- 2.;
  let det = determinant m in
  assert_equal 17. det

let test_submatrix_three _ =
  let m = Array.make_matrix 3 3 0. in
  m.(0).(0) <- 1.;
  m.(0).(1) <- 5.;
  m.(0).(2) <- 0.;
  m.(1).(0) <- (-3.);
  m.(1).(1) <- (2.);
  m.(1).(2) <- (7.);
  m.(2).(0) <- 0.;
  m.(2).(1) <- 6.;
  m.(2).(2) <- (-3.); 
  let twobytwo =  Array.make_matrix 2 2 0. in
  twobytwo.(0).(0) <- (-3.);
  twobytwo.(0).(1) <- 2.;
  twobytwo.(1).(0) <- 0.;
  twobytwo.(1).(1) <- 6.;
  let res = submatrix m 0 2 in
  (* let () = res |> Array.iter (Array.iter print_float) in *)
  assert_bool "three by three to two by two submatrix" (twobytwo = res)

let test_submatrix_four _ =
  let m1 = Array.make_matrix 4 4 0. in
  m1.(0).(0) <- (-6.);
  m1.(0).(1) <- 1.;
  m1.(0).(2) <- 1.;
  m1.(0).(3) <- 6.;
  m1.(1).(0) <- (-8.);
  m1.(1).(1) <- 5.;
  m1.(1).(2) <- 8.;
  m1.(1).(3) <- 6.;
  m1.(2).(0) <- (-1.);
  m1.(2).(1) <- 0.;
  m1.(2).(2) <- 8.;
  m1.(2).(3) <- 2.;
  m1.(3).(0) <- (-7.);
  m1.(3).(1) <- 1.;
  m1.(3).(2) <- (-1.);
  m1.(3).(3) <- 1.; 
  let sub = submatrix m1 2 1 in 
  let m2 = Array.make_matrix 3 3 0. in
  m2.(0).(0) <- (-6.);
  m2.(0).(1) <- 1.;
  m2.(0).(2) <- 6.;
  m2.(1).(0) <- (-8.);
  m2.(1).(1) <- 8.;
  m2.(1).(2) <- 6.;
  m2.(2).(0) <- (-7.);
  m2.(2).(1) <- (-1.);
  m2.(2).(2) <- 1.;
  (* let () = sub |> Array.iter (Array.iter print_float) in
  let () = m2 |> Array.iter (Array.iter print_float) in *)
  assert_bool "four to three" (m2 = sub)

let test_three_three_minor _ =
  let m = Array.make_matrix 3 3 0. in
  m.(0).(0) <- 3.;
  m.(0).(1) <- 5.;
  m.(0).(2) <- 0.;
  m.(1).(0) <- (2.);
  m.(1).(1) <- (-1.);
  m.(1).(2) <- (-7.);
  m.(2).(0) <- 6.;
  m.(2).(1) <- (-1.);
  m.(2).(2) <- (5.); 
  let b = submatrix m 1 0 in
  let det = determinant b in
  assert_equal 25. det;
  assert_equal 25. (minor m 1 0)

let test_three_three_cofactor _ =
  let m = Array.make_matrix 3 3 0. in
  m.(0).(0) <- 3.;
  m.(0).(1) <- 5.;
  m.(0).(2) <- 0.;
  m.(1).(0) <- (2.);
  m.(1).(1) <- (-1.);
  m.(1).(2) <- (-7.);
  m.(2).(0) <- 6.;
  m.(2).(1) <- (-1.);
  m.(2).(2) <- (5.); 
  assert_equal (-12.) (minor m 0 0);
  assert_equal (-12.) (cofactor m 0 0);
  assert_equal (25.) (minor m 1 0);
  assert_equal (-25.) (cofactor m 1 0)

let test_three_three_determinant _ =
  let m = Array.make_matrix 3 3 0. in
  m.(0).(0) <- 1.;
  m.(0).(1) <- 2.;
  m.(0).(2) <- 6.;
  m.(1).(0) <- (-5.);
  m.(1).(1) <- (8.);
  m.(1).(2) <- (-4.);
  m.(2).(0) <- 2.;
  m.(2).(1) <- (6.);
  m.(2).(2) <- (4.); 
  assert_equal 56. (cofactor m 0 0);
  assert_equal 12. (cofactor m 0 1);
  assert_equal (-46.) (cofactor m 0 2);
  assert_equal (-196.) (determinant m)

let test_four_four_determinant _ =
  let m1 = Array.make_matrix 4 4 0. in
  m1.(0).(0) <- (-2.);
  m1.(0).(1) <- (-8.);
  m1.(0).(2) <- 3.;
  m1.(0).(3) <- 5.;
  m1.(1).(0) <- (-3.);
  m1.(1).(1) <- 1.;
  m1.(1).(2) <- 7.;
  m1.(1).(3) <- 3.;
  m1.(2).(0) <- (1.);
  m1.(2).(1) <- 2.;
  m1.(2).(2) <- (-9.);
  m1.(2).(3) <- 6.;
  m1.(3).(0) <- (-6.);
  m1.(3).(1) <- 7.;
  m1.(3).(2) <- (7.);
  m1.(3).(3) <- (-9.);  
  assert_equal 690. (cofactor m1 0 0);
  assert_equal 447. (cofactor m1 0 1);
  assert_equal (210.) (cofactor m1 0 2);
  assert_equal (51.) (cofactor m1 0 3);
  assert_equal (-4071.) (determinant m1)

let test_invertible _ =
  let m1 = Array.make_matrix 4 4 0. in
  m1.(0).(0) <- (6.);
  m1.(0).(1) <- (4.);
  m1.(0).(2) <- 4.;
  m1.(0).(3) <- 4.;
  m1.(1).(0) <- (5.);
  m1.(1).(1) <- 5.;
  m1.(1).(2) <- 7.;
  m1.(1).(3) <- 6.;
  m1.(2).(0) <- (4.);
  m1.(2).(1) <- (-9.);
  m1.(2).(2) <- (3.);
  m1.(2).(3) <- (-7.);
  m1.(3).(0) <- (9.);
  m1.(3).(1) <- 1.;
  m1.(3).(2) <- (7.);
  m1.(3).(3) <- (-6.);
  assert_equal (-2120.) (determinant m1);
  assert_bool "invertible" (invertible m1)

let test_not_invertible _ =
  let m1 = Array.make_matrix 4 4 0. in
  m1.(0).(0) <- (-4.);
  m1.(0).(1) <- (2.);
  m1.(0).(2) <- (-2.);
  m1.(0).(3) <- (-3.);
  m1.(1).(0) <- (9.);
  m1.(1).(1) <- 6.;
  m1.(1).(2) <- 2.;
  m1.(1).(3) <- 6.;
  m1.(2).(0) <- (0.);
  m1.(2).(1) <- (-5.);
  m1.(2).(2) <- (1.);
  m1.(2).(3) <- (-5.);
  m1.(3).(0) <- (0.);
  m1.(3).(1) <- 0.;
  m1.(3).(2) <- (0.);
  m1.(3).(3) <- 0.;
  assert_equal (0.) (determinant m1);
  assert_bool "shouldn't be invertible" (not (invertible m1))


let test_inverse_building_blocks _ =
  let m1 = Array.make_matrix 4 4 0. in
  m1.(0).(0) <- (-5.);
  m1.(0).(1) <- (2.);
  m1.(0).(2) <- (6.);
  m1.(0).(3) <- (-8.);
  m1.(1).(0) <- (1.);
  m1.(1).(1) <- (-5.);
  m1.(1).(2) <- 1.;
  m1.(1).(3) <- 8.;
  m1.(2).(0) <- (7.);
  m1.(2).(1) <- (7.);
  m1.(2).(2) <- (-6.);
  m1.(2).(3) <- (-7.);
  m1.(3).(0) <- (1.);
  m1.(3).(1) <- (-3.);
  m1.(3).(2) <- (7.);
  m1.(3).(3) <- 4.;

  let m2 = Array.make_matrix 4 4 0. in
  m2.(0).(0) <- (0.21805);
  m2.(0).(1) <- (0.45113);
  m2.(0).(2) <- (0.24060);
  m2.(0).(3) <- (-0.04511 );
  m2.(1).(0) <- (-0.80827 );
  m2.(1).(1) <- (-1.45677);
  m2.(1).(2) <- (-0.44361);
  m2.(1).(3) <- 0.52068;
  m2.(2).(0) <- (-0.07895);
  m2.(2).(1) <- (-0.22368);
  m2.(2).(2) <- (-0.05263);
  m2.(2).(3) <- (0.19737);
  m2.(3).(0) <- (-0.52256 );
  m2.(3).(1) <- (-0.81391);
  m2.(3).(2) <- (-0.30075);
  m2.(3).(3) <- 0.30639;
  (* let c = (cofactor m1 3 3) in
  let k = c /. (determinant m1) in *)
  (* assert_equal (0.30639) k; *)
  assert_equal (532.) (determinant m1);
  assert_equal (-160.) (cofactor m1 2 3);
  assert_equal (105.) (cofactor m1 3 2);
  (* Printf.printf "Cofactor 3 3";
  print_float (cofactor m1 3 3) *)
  assert_equal (163.) (cofactor m1 3 3)


let test_invertible _ =
  let m1 = Array.make_matrix 4 4 0. in
  m1.(0).(0) <- (6.);
  m1.(0).(1) <- (4.);
  m1.(0).(2) <- 4.;
  m1.(0).(3) <- 4.;
  m1.(1).(0) <- (5.);
  m1.(1).(1) <- 5.;
  m1.(1).(2) <- 7.;
  m1.(1).(3) <- 6.;
  m1.(2).(0) <- (4.);
  m1.(2).(1) <- (-9.);
  m1.(2).(2) <- (3.);
  m1.(2).(3) <- (-7.);
  m1.(3).(0) <- (9.);
  m1.(3).(1) <- 1.;
  m1.(3).(2) <- (7.);
  m1.(3).(3) <- (-6.);
  assert_equal (-2120.) (determinant m1);
  assert_bool "invertible" (invertible m1)

let test_not_invertible _ =
  let m1 = Array.make_matrix 4 4 0. in
  m1.(0).(0) <- (-4.);
  m1.(0).(1) <- (2.);
  m1.(0).(2) <- (-2.);
  m1.(0).(3) <- (-3.);
  m1.(1).(0) <- (9.);
  m1.(1).(1) <- 6.;
  m1.(1).(2) <- 2.;
  m1.(1).(3) <- 6.;
  m1.(2).(0) <- (0.);
  m1.(2).(1) <- (-5.);
  m1.(2).(2) <- (1.);
  m1.(2).(3) <- (-5.);
  m1.(3).(0) <- (0.);
  m1.(3).(1) <- 0.;
  m1.(3).(2) <- (0.);
  m1.(3).(3) <- 0.;
  assert_equal (0.) (determinant m1);
  assert_bool "shouldn't be invertible" (not (invertible m1))


let test_inverse _ =
  let m1 = Array.make_matrix 4 4 0. in
  m1.(0).(0) <- (-5.);
  m1.(0).(1) <- (2.);
  m1.(0).(2) <- (6.);
  m1.(0).(3) <- (-8.);
  m1.(1).(0) <- (1.);
  m1.(1).(1) <- (-5.);
  m1.(1).(2) <- 1.;
  m1.(1).(3) <- 8.;
  m1.(2).(0) <- (7.);
  m1.(2).(1) <- (7.);
  m1.(2).(2) <- (-6.);
  m1.(2).(3) <- (-7.);
  m1.(3).(0) <- (1.);
  m1.(3).(1) <- (-3.);
  m1.(3).(2) <- (7.);
  m1.(3).(3) <- 4.;

  let m2 = Array.make_matrix 4 4 0. in
  m2.(0).(0) <- (0.21805);
  m2.(0).(1) <- (0.45113);
  m2.(0).(2) <- (0.24060);
  m2.(0).(3) <- (-0.04511 );
  m2.(1).(0) <- (-0.80827 );
  m2.(1).(1) <- (-1.45677);
  m2.(1).(2) <- (-0.44361);
  m2.(1).(3) <- 0.52068;
  m2.(2).(0) <- (-0.07895);
  m2.(2).(1) <- (-0.22368);
  m2.(2).(2) <- (-0.05263);
  m2.(2).(3) <- (0.19737);
  m2.(3).(0) <- (-0.52256 );
  m2.(3).(1) <- (-0.81391);
  m2.(3).(2) <- (-0.30075);
  m2.(3).(3) <- 0.30639; 
  let b = inverse m1 in
  (* let () = m2 |> Array.iter (Array.iter print_float) in *)
  Printf.printf "\n\n";
  (* let () = b |> Array.iter (Array.iter print_float) in *)
  assert_bool "Inverse" (matrix_compare m2 b);
  assert_equal (532.) (determinant m1);
  assert_equal (-160.) (cofactor m1 2 3);
  assert_equal (105.) (cofactor m1 3 2)

let test_inverse_2 _ =
  let m1 = Array.make_matrix 4 4 0. in
  m1.(0).(0) <- (8.);
  m1.(0).(1) <- (-5.);
  m1.(0).(2) <- (9.);
  m1.(0).(3) <- (2.);
  m1.(1).(0) <- (7.);
  m1.(1).(1) <- (5.);
  m1.(1).(2) <- 6.;
  m1.(1).(3) <- 1.;
  m1.(2).(0) <- (-6.);
  m1.(2).(1) <- (0.);
  m1.(2).(2) <- (9.);
  m1.(2).(3) <- (6.);
  m1.(3).(0) <- (-3.);
  m1.(3).(1) <- (0.);
  m1.(3).(2) <- (-9.);
  m1.(3).(3) <- (-4.);

  let m2 = Array.make_matrix 4 4 0. in
  m2.(0).(0) <- (-0.15385);
  m2.(0).(1) <- (-0.15385);
  m2.(0).(2) <- (-0.28205);
  m2.(0).(3) <- (-0.53846 );
  m2.(1).(0) <- (-0.07692 );
  m2.(1).(1) <- (0.12308);
  m2.(1).(2) <- (0.02564);
  m2.(1).(3) <- 0.03077;
  m2.(2).(0) <- (0.35897);
  m2.(2).(1) <- (0.35897);
  m2.(2).(2) <- (0.43590);
  m2.(2).(3) <- (0.92308);
  m2.(3).(0) <- (-0.69231);
  m2.(3).(1) <- (-0.69231);
  m2.(3).(2) <- (-0.76923);
  m2.(3).(3) <- (-1.92308); 
  let b = inverse m1 in
  assert_bool "Inverse" (matrix_compare m2 b)

let test_inverse_3 _ =
  let m1 = Array.make_matrix 4 4 0. in
    m1.(0).(0) <- (9.);
    m1.(0).(1) <- (3.);
    m1.(0).(2) <- (0.);
    m1.(0).(3) <- (9.);
    m1.(1).(0) <- (-5.);
    m1.(1).(1) <- (-2.);
    m1.(1).(2) <- (-6.);
    m1.(1).(3) <- (-3.);
    m1.(2).(0) <- (-4.);
    m1.(2).(1) <- (9.);
    m1.(2).(2) <- (6.);
    m1.(2).(3) <- (4.);
    m1.(3).(0) <- (-7.);
    m1.(3).(1) <- (6.);
    m1.(3).(2) <- (6.);
    m1.(3).(3) <- (2.);
  
    let m2 = Array.make_matrix 4 4 0. in
    m2.(0).(0) <- (-0.04074);
    m2.(0).(1) <- (-0.07778);
    m2.(0).(2) <- (0.14444);
    m2.(0).(3) <- (-0.22222 );
    m2.(1).(0) <- (-0.07778 );
    m2.(1).(1) <- (0.03333);
    m2.(1).(2) <- (0.36667);
    m2.(1).(3) <-(-0.33333 );
    m2.(2).(0) <- (-0.02901);
    m2.(2).(1) <- (-0.14630);
    m2.(2).(2) <- (-0.10926);
    m2.(2).(3) <- (0.12963);
    m2.(3).(0) <- (0.17778);
    m2.(3).(1) <- (0.06667 );
    m2.(3).(2) <- (-0.26667);
    m2.(3).(3) <- (0.33333); 
    let b = inverse m1 in
    assert_bool "Inverse" (matrix_compare m2 b)
  
  

let suite =
  "MatrixTestList" >::: [
    "test_4_by_4_creation" >:: test_4_by_4_creation;
    "test_2_by_2_creation" >:: test_2_by_2_creation;
    "test_3_by_3_creation" >:: test_3_by_3_creation;
    "test_matrix_equality" >:: test_matrix_equality;
    "test_matrix_inequality" >:: test_matrix_inequality;
    "test_matrix_mult" >:: test_matrix_mult;
    "test_tuple_matrix_mult" >:: test_tuple_matrix_mult;
    "test_identity_matrix" >:: test_identity_matrix;
    "test_transpose" >:: test_transpose;
    "trans_identity" >:: trans_identity;
    "test_determination" >:: test_determination;
    "test_submatrix_three" >:: test_submatrix_three;
    "test_submatrix_four" >:: test_submatrix_four;
    "test_three_three_minor" >:: test_three_three_minor;
    "test_three_three_cofactor" >:: test_three_three_cofactor;
    "test_three_three_determinant" >:: test_three_three_determinant;
    "test_four_four_determinant" >:: test_four_four_determinant;
    "test_invertible" >:: test_invertible;
    "test_not_invertible" >:: test_not_invertible;
    "test_inverse_building_blocks" >:: test_inverse_building_blocks;
    "test_inverse" >:: test_inverse;
    "test_inverse_2" >:: test_inverse_2;
    "test_inverse_3" >:: test_inverse_3;
  ]

let () =
  run_test_tt_main suite