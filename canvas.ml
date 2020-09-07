open Color


class canvas height width = 
  object(self)
    val pixels_r = Array.make height (Array.make width 0.)
    val pixels_g = Array.make height (Array.make width 0.)
    val pixels_b = Array.make height (Array.make width 0.)
    method pixel (x: int) (y:int) =
      {r= pixels_r.(x).(y); g=pixels_g.(x).(y); b=pixels_b.(x).(y)}
    method height =
      height
    method width =
      width
    method write_pixel (x: int) (y:int) (c: color) =
      pixels_r.(x).(y) <- c.r;
      pixels_g.(x).(y) <- c.g;
      pixels_b.(x).(y) <- c.b
  end