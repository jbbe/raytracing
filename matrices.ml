open Tuple


let matrix_mult (a: float array array) (b: float array array) : float array array =
  let res = Array.make_matrix 4 4 0. in
  for i = 0 to 3 do
    for j = 0 to 3 do
      res.(i).(j) <- ((a.(i).(0) *. b.(0).(j)) +. (a.(i).(1) *. b.(1).(j)) +. (a.(i).(2) *. b.(2).(j)) +.(a.(i).(3) *. b.(3).(j)));
    done;
  done;
  res

let matrix_tuple_mult (m: float array array) (t: tuple) : tuple =
  {
    x=((m.(0).(0) *. t.x) +. (m.(0).(1) *. t.y) +. (m.(0).(2) *. t.z) +. (m.(0).(3) *. t.w));
    y=(m.(1).(0) *. t.x) +. (m.(1).(1) *. t.y) +. (m.(1).(2) *. t.z) +. (m.(1).(3) *. t.w);
    z=(m.(2).(0) *. t.x) +. (m.(2).(1) *. t.y) +. (m.(2).(2) *. t.z) +. (m.(2).(3) *. t.w);
    w=(m.(3).(0) *. t.x) +. (m.(3).(1) *. t.y) +. (m.(3).(2) *. t.z) +. (m.(3).(3) *. t.w);
  }

let transpose (m: float array array) : float array array =
  let trans = Array.make_matrix 4 4 0. in
  for i = 0 to 3 do
    for j = 0 to 3 do
      trans.(i).(j) <- m.(j).(i)
    done;
  done;
  trans

let make_identity _ =
  let id = Array.make_matrix 4 4 0. in
  id.(0).(0) <- 1.;
  id.(0).(1) <- 0.;
  id.(0).(2) <- 0.;
  id.(0).(3) <- 0.;
  id.(1).(0) <- 0.;
  id.(1).(1) <- 1.;
  id.(1).(2) <- 0.;
  id.(1).(3) <- 0.;
  id.(2).(0) <- 0.;
  id.(2).(1) <- 0.;
  id.(2).(2) <- 1.;
  id.(2).(3) <- 0.;
  id.(3).(0) <- 0.;
  id.(3).(1) <- 0.;
  id.(3).(2) <- 0.;
  id.(3).(3) <- 1.;
  id

let identity_matrix = make_identity ()

let determinant (m: float array array) : float =
  (m.(0).(0) *. m.(1).(1)) -. (m.(0).(1) *. m.(1).(0))

let submatrix (m: float array array) (row: int) (col: int): float array array =
  let sub = Array.make_matrix ((Array.length m) - 1) ((Array.length m.(0)) - 1) 0. 
  and row_idx = ref 0 
  and col_idx = ref 0 in
  for i = 0 to ((Array.length m) - 1) do
    if i != row then (
      for j = 0 to ((Array.length m.(0)) - 1) do
        if j != col then(
          sub.(!row_idx).(!col_idx) <- m.(i).(j);
          col_idx := (!col_idx + 1);)
      done; 
      row_idx := (!row_idx + 1);
      col_idx := 0;)
  done;
  sub