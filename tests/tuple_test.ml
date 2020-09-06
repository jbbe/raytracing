open OUnit2
open Tuple

(* type tuple = {x: float; y: float; z:float; w:float} *)

let point_a = {x=4.3; y=(-4.2);  z=3.1; w=1.}

let vector_a = {x=4.3; y=(-4.2);  z=3.1; w=0.}

let test_point_tuple _ =
  (* Check if members are as they should be. *)
    assert_equal 4.3 point_a.x;
    assert_equal (-4.2) point_a.y;
    assert_equal 3.1 point_a.z;
    assert_equal 1. point_a.w;
    assert_bool "a_point is a point" (is_point point_a);
    assert_bool "a_point is not a vector" (not (is_vector point_a))

let test_vector_tuple _ =
  (* Check if members are as they should be. *)
    assert_equal 4.3 vector_a.x;
    assert_equal (-4.2) vector_a.y;
    assert_equal 3.1 vector_a.z;
    assert_equal 0. vector_a.w;
    assert_bool "a_vector is not point" (not (is_point vector_a));
    assert_bool "a_vector is a vector" (is_vector vector_a)


let test_tuple_equal _ =
  assert_bool "point_a and point_a" (tuple_equal point_a point_a);
  assert_bool "vec a and veca " (tuple_equal vector_a vector_a);
  assert_bool "point a and a new 1. of itself" (tuple_equal point_a {x=4.3; y=(-4.2);  z=3.1; w=1.});
  assert_bool "Vec a and a fresh vec" (tuple_equal vector_a {x=4.3; y=(-4.2);  z=3.1; w=0.})


let test_tuple_not_equal _ = 
  assert_bool "point a and vec a" (not (tuple_equal point_a vector_a))


let test_float_equal _ =
  assert_bool "Same val" (float_equal 0.1 0.1);
  assert_bool "Within epsiolon" (float_equal 0.1 0.1000001);
  assert_bool "not equal" (not (float_equal 0.1 0.2))

let a1 = {x=3.; y=(-2.); z=5.; w=1.} 
let a2 = {x=(-2.); y=3.; z=1.; w=0.}
let test_adding_tuples _ =
  assert_bool "Adding tuples" (tuple_equal (tuple_add a1 a2) {x=1.; y=1.; z=6.; w=1.} )

let p1 = {x=3.; y=2.; z=1.; w=1.}
let p2 = {x=5.; y=6.; z=7.; w=1.}
let v1 = {x=3.; y=2.; z=1.; w=0.}
let v2= {x=5.; y=6.; z=7.; w=0.}
let test_sub_p_from_p _ =
  let diff = {x=(-2.); y=(-4.); z=(-6.); w=0.}in
  assert_bool "Sub p from p" (tuple_equal (tuple_sub p1 p2) diff)

let test_sub_v_from_p _ =
  let diff =  {x=(-2.); y=(-4.); z=(-6.); w=1.} in
    assert_bool "Sub v from p" (tuple_equal (tuple_sub p1 v2) diff)

let test_sub_v_from_v _ =
  let diff =  {x=(-2.); y=(-4.); z=(-6.); w=0.} in
    assert_bool "Sub v from v" (tuple_equal (tuple_sub v1 v2) diff)

let test_tuple_negation _ =
  let zero_vec ={x=0.;y=0.;z=0.;w=0.} in
    let v = {x=1.; y=(-2.); z=3.; w=0.} in
      assert_bool "Tuple negate" (tuple_equal (tuple_sub zero_vec v) (negate v));
      assert_bool "Tuple negate" (tuple_equal {x=(-1.); y=(2.); z=(-3.); w=0.} (negate v))

let test_scalar_mult _ =
  let a = {x=1.; y= (-2.); z=3.; w=(-4.)} in 
    let res = {x=3.5; y=(-7.); z=10.5; w=(-14.)} in
      assert_bool "scalar Mult" (tuple_equal (scalar_mult a 3.5) res)

let test_fractional_mult _ =
  let a = {x=1.; y= (-2.); z=3.; w=(-4.)} in 
    let res = {x=0.5; y=(-1.); z=1.5; w=(-2.)} in
      assert_bool "fractional Mult" (tuple_equal (scalar_mult a 0.5) res)
 
 
(* let test_sub_p_from_v _ =
  assert_raises Tuple.ValueError("Cannot add two points.") (Tuple.tuple_sub v1 p2) *)

let suite =
  "TupleTestList" >::: [
    "test_point_tuple_x" >:: test_point_tuple;
    "test_vector_tuple" >:: test_vector_tuple;
    "test_tuple_equal" >:: test_tuple_equal;
    "test_tuple_not_equal" >:: test_tuple_not_equal;
    "test_float_equal" >:: test_float_equal;
    "test_adding_tuples" >:: test_adding_tuples;
    "test_sub_p_from_p" >:: test_adding_tuples;
    "test_sub_v_from_p" >:: test_adding_tuples;
    "test_sub_v_from_v" >:: test_adding_tuples;
    "test_tuple_negation" >:: test_tuple_negation;
    "test_scalar_mult" >:: test_scalar_mult;
    "test_fractional_mult" >:: test_fractional_mult;
  ]

let () =
  run_test_tt_main suite
