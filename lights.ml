open Color
open Tuple

type light = {intensity:color; position:tuple}

type material = {
  mutable color: color;
  mutable ambient: float;
  mutable diffuse: float;
  mutable specular: float;
  mutable shininess: float;
}

let point_light _position _intensity =
  {intensity= _intensity; position=_position}

let default_material _ =
  {color={r=1.; g=1.; b=1.}; 
    ambient=0.1; 
    diffuse=0.9;
    specular=0.9;
    shininess=200.
    }

let lighting _material _light _point _eyev _normalv =
  (* combine surface color with the light's color/intensity *)
  let effective_color = schur_prod _material.color _light.intensity in

  (* find the direction of light source *)
  let lightv = normalize (tuple_sub _light.position _point) in

  (* compute ambient contribution *)
  let ambient = color_scalar_mult effective_color _material.ambient in

  (* light_dot_normal is cosine of angle between light vec and normal vec
  negative value means the light is on the other side of the surface *)
  let light_dot_normal = dot lightv _normalv in

  if light_dot_normal < 0. 
    then color_add (color_add (black ()) (black ())) ambient
    else 
      let diffuse = color_scalar_mult (color_scalar_mult effective_color _material.diffuse) light_dot_normal in
      (* reflect_dot_eye represents the cosine of the angle between the​
        reflection vector and the eye vector. A negative number means the​
        light reflects away from the eye.​ *)
      let reflectv = reflect (scalar_mult lightv (-1.)) _normalv in
      let reflect_dot_eye = dot reflectv _eyev in
      let specular = (if reflect_dot_eye <= 0. 
        then black () 
        else color_scalar_mult (color_scalar_mult _light.intensity _material.specular) (reflect_dot_eye ** _material.shininess)
      ) in
      color_add (color_add ambient diffuse) specular
  