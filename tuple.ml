type tuple = {x: float; y: float; z:float; w:float}

exception ValueError of string

let float_equal (f1: float) (f2: float) : bool =
  (Float.abs (f1 -. f2)) <= 0.0001;;

let is_point (t: tuple) =
  float_equal t.w 1.

let is_vector (t: tuple) =
  float_equal t.w 0.



let tuple_equal (t1: tuple ) (t2: tuple) : bool =
  (float_equal t1.x t2.x) && (float_equal t1.y t2.y) && (float_equal t1.z t2.z) && (float_equal t1.w t2.w)

let tuple_add (t1: tuple ) (t2: tuple) : tuple =
  if (float_equal t1.w 1.) && (float_equal t2.w 1.) 
    then raise (ValueError "Cannot add two points.")
    else {x= t1.x +. t2.x; y=t1.y +. t2.y; z= t1.z +. t2.z; w = t1.w +. t2.w}

let tuple_sub (t1: tuple ) (t2: tuple) : tuple =
  if (float_equal t1.w 0.) && (float_equal t2.w 1.) 
    then raise (ValueError "Cannot subtract a point from a vector.")
    else {x= t1.x -. t2.x; y=t1.y -. t2.y; z= t1.z -. t2.z; w = t1.w -. t2.w}


let zero_vec = {x=0.; y=0.; z=0.; w=0.}
let negate (t: tuple) : tuple = 
  tuple_sub zero_vec t

let scalar_mult (t: tuple) (scalar: float) : tuple =
  if (float_equal t.w 1.)
    then raise (ValueError "Cannot multiply a point.")
    else {x=t.x *. scalar; y=t.y *. scalar; z= t.z *. scalar; w=t.w *. scalar}

let scalar_div (t: tuple) (scalar: float) : tuple =
  if (float_equal scalar 0.) 
    then raise (ValueError "Cannot divide by 0.")
    else  {x=t.x /. scalar; y=t.y /. scalar; z= t.z /. scalar; w=t.w /. scalar}

let magnitude (t: tuple) : float =
  sqrt ((t.x *. t.x) +. (t.y *. t.y) +. (t.z *. t.z) +. (t.w *. t.w))