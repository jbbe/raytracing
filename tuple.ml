type w = 
  | Zero 
  | One
(* type w = float
type x = float
type y = float
type z = float *)

type tuple = 
  | Point of float * float * float * w
  | Vector of float * float * float * w


let float_equal (f1: float) (f2: float) : bool =
  (Float.abs (f1 -. f2)) <= 0.0001;;

let tuple_equal (t1: tuple ) (t2: tuple) : bool =
  match t1 with
  | Point (x1, y1, z1, w1) -> (match t2 with
      | Point (x2, y2, z2, w2) -> 
        (float_equal x1 x2) && (float_equal y1 y2) && (float_equal z1 z2)
      | _ -> false)
  | Vector (x1, y1, z1, w1) -> (match t2 with
    | Vector (x2, y2, z2, w2) -> 
      (float_equal x1 x2) && (float_equal y1 y2) && (float_equal z1 z2)
    | _ -> false)


