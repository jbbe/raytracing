open Color
open Tuple

type stripe_struct = {a: color; b: color;}

let stripe_pattern a_color b_color =
  {a=a_color; b=b_color}

let stripe_at (_pattern : stripe_struct) (_point : tuple) : color =
  let a_or_b =  mod_float (Float.abs _point.x) 2. in
  if a_or_b < 1. then _pattern.a else _pattern.b