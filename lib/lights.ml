open Color
open Matrices
(* open Shapes *)
open Tuple
open Printf

type light = {intensity:color; position:tuple}

type pattern_type = Solid | Stripe | Gradient | Ring | Checkers

class pattern pattern_type_in colors_in =
 object (self)
    val _pattern_type : pattern_type = pattern_type_in
    val mutable _colors = colors_in
    val mutable _transform = identity_matrix
    method pattern_type = _pattern_type
    method set_transform (m :float array array) = _transform <- m
    method add_transform (m :float array array) = _transform <- matrix_mult _transform m
    method transform = _transform
    method first_color = (List.nth _colors 0)
    method second_color = if (List.length _colors) < 2 then (List.nth _colors 0) else (List.nth _colors 1)
    method num_colors = List.length _colors
    method color_at (_point : tuple) : color =
      match _pattern_type with
      | Solid -> (List.hd _colors)
      | Stripe ->  (let a_or_b =  mod_float (Float.abs _point.x) 2. in
        let idx = if (_point.x < 0.) then (if a_or_b <= 1. then 1 else 0)
                  else if a_or_b < 1. then 0 else 1 in
        (List.nth _colors idx))
      | Gradient -> (let distance = color_sub self#second_color self#first_color in
                    let fraction = _point.x -. (Float.floor _point.x) in
                    color_add self#first_color (color_scalar_mult distance fraction))
      | Ring -> let dist = (int_of_float (sqrt ((_point.x *. _point.x) +. (_point.z *. _point.z)))) in
                if (dist mod  2) = 0 then self#first_color else self#second_color
      | Checkers -> let dist = ((int_of_float (Float.abs _point.x)) 
                    + (int_of_float (Float.abs _point.y)) 
                    + (int_of_float (Float.abs _point.z))) in
                 if (dist mod 2) = 0 then self#first_color else self#second_color
end


type material = {
  mutable ambient: float;
  mutable diffuse: float;
  mutable specular: float;
  mutable shininess: float;
  mutable reflective: float;
  mutable pattern: pattern;
}

let point_light _position _intensity =
  {intensity= _intensity; position=_position}

let print_light l =
  Printf.printf "Print light";
  print_color l.intensity;
  print_tuple l.position

let default_material _ =
  { 
    ambient=0.1; 
    diffuse=0.9;
    specular=0.9;
    shininess=200.;
    reflective=0.;
    pattern=(new pattern Solid [{r=1.; g=1.; b=1.}]);
    }

let pattern_equal p1 p2 : bool =
  match p1#pattern_type with 
  | Solid -> (match p2#pattern_type with 
      | Solid -> (p1#first_color = p2#first_color)
      | _ -> false)
  | Stripe -> (match p2#pattern_type with 
      | Stripe -> (p1#first_color = p2#first_color) && (p1#first_color = p2#first_color)
      | _ -> false)
  | Gradient -> (match p2#pattern_type with 
      | Gradient -> (p1#first_color = p2#first_color) && (p1#first_color = p2#first_color)
      | _ -> false)
  | Ring -> (match p2#pattern_type with 
      | Ring -> (p1#first_color = p2#first_color) && (p1#first_color = p2#first_color)
      | _ -> false)
  | Checkers -> (match p2#pattern_type with 
      | Checkers -> (p1#first_color = p2#first_color) && (p1#first_color = p2#first_color)
      | _ -> false)
  

let material_equal m1 m2 : bool =
  (m1.ambient = m2.ambient) 
    && (m1.ambient = m2.ambient) 
    && (m1.diffuse = m2.diffuse)
    && (m1.specular = m2.specular)
    && (m1.shininess = m2.shininess)
    && (m1.reflective = m2.reflective)
    && (pattern_equal m1.pattern m2.pattern)

let pattern_type_as_string p =
  match p with
  | Checkers -> "checkers"
  | Solid -> "solid"
  | Stripe -> "stripe" 
  | Ring -> "ring"
  | Gradient -> "gradient"

let print_material m =
  printf "\nMaterial ambient= %f spec = %f diff = %f shiny= %f reflec= %f pattern = %s " 
     m.ambient m.specular m.diffuse m.shininess m.reflective (pattern_type_as_string (m.pattern#pattern_type));
  print_color m.pattern#first_color



let stripe_pattern a_color b_color =
  new pattern Stripe [a_color; b_color]


