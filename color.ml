type color = {r: float; g: float; b: float}

let float_equal (f1: float) (f2: float) : bool =
  (Float.abs (f1 -. f2)) <= 0.0001;;

let color_add (c1: color) (c2: color) : color =
  {r=c1.r +. c2.r; g=c1.g +. c2.g; b=c1.b +. c2.b}

let color_sub (c1: color) (c2: color) : color =
  {r=c1.r -. c2.r; g=c1.g -. c2.g; b=c1.b -. c2.b}

let schur_prod (c1: color) (c2: color) : color =
  {r=c1.r *. c2.r; g=c1.g *. c2.g; b=c1.b *. c2.b}

let color_scalar_mult (c: color) (scalar: float) : color =
  {r=c.r *. scalar; g=c.g *. scalar; b=c.b *. scalar}

let color_equal (c1: color) (c2:color) : bool =
  (float_equal c1.r c2.r) && (float_equal c1.g c2.g) && (float_equal c1.b c2.b)