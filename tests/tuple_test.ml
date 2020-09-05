open OUnit2
open Tuple

let point_a = Point (4.3, -4.2, 3.1, One)

let vector_a = Vector (4.3, -4.2, 3.1, Zero)
let list_a = [1;2;3]

let test_point_tuple_x _ =
  (* Check if members are as they should be. *)
  match point_a with 
    | Point (x, y, z, w) -> assert_equal 4.3 x
    | Vector (x, y, z, w) -> assert_equal 1 0

let test_point_tuple_y _ =  
  match point_a with 
    | Point (x, y, z, w) -> assert_equal (-4.2) y
    | Vector (x, y, z, w) -> assert_equal 1 0

let test_point_tuple_z _ =  
  match point_a with 
    | Point (x, y, z, w) -> assert_equal 3.1 z
    | Vector (x, y, z, w) -> assert_equal 1 0

let test_point_tuple_w _ =  
  match point_a with 
    | Point (x, y, z, w) -> assert_equal One w
    | Vector (x, y, z, w) -> assert_equal 1 0


let test_vector_tuple_x _ =
  (* Check if members are as they should be. *)
  match vector_a with 
    | Point (x, y, z, w) -> assert_equal 1 0
    | Vector (x, y, z, w) -> assert_equal 4.3 x

let test_vector_tuple_y _ =  
  match vector_a with 
    | Point (x, y, z, w) -> assert_equal 1 0
    | Vector (x, y, z, w) -> assert_equal (-4.2) y

let test_vector_tuple_z _ =  
  match vector_a with 
    | Point (x, y, z, w) -> assert_equal 1 0
    | Vector (x, y, z, w) -> assert_equal 3.1 z

let test_vector_tuple_w _ =  
  match vector_a with 
    | Point (x, y, z, w) -> assert_equal 1 0
    | Vector (x, y, z, w) ->  assert_equal Zero w

let test_tuple_equal _ =
  assert_bool "point_a and point_a" (tuple_equal point_a point_a);
  assert_bool "vec a and veca " (tuple_equal vector_a vector_a);
  assert_bool "point a and a new one of itself" (tuple_equal point_a (Point (4.3, -4.2, 3.1, One)));
  assert_bool "Vec a and a fresh vec" (tuple_equal vector_a (Vector (4.3, -4.2, 3.1, Zero)))

let test_tuple_not_equal _ = 
  assert_bool "point a and vec a" (not (tuple_equal point_a vector_a))

let test_float_equal _ =
  assert_bool "Same val" (float_equal 0.1 0.1);
  assert_bool "Within epsiolon" (float_equal 0.1 0.1000001);
  assert_bool "not equal" (not (float_equal 0.1 0.2))
  


let suite =
  "TupleTestList" >::: [
    "test_point_tuple_x" >:: test_point_tuple_x;
    "test_point_tuple_y" >:: test_point_tuple_y;
    "test_point_tuple_z" >:: test_point_tuple_z;
    "test_point_tuple_w" >:: test_point_tuple_y;
    "test_vector_tuple_x" >:: test_vector_tuple_x;
    "test_vector_tuple_y" >:: test_vector_tuple_y;
    "test_vector_tuple_z" >:: test_vector_tuple_z;
    "test_vector_tuple_w" >:: test_vector_tuple_y;
    "test_tuple_equal" >:: test_tuple_equal;
    "test_float_equal" >:: test_float_equal;
  ]

let () =
  run_test_tt_main suite
