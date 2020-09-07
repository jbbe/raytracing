open Color

exception ValueError of string
class canvas height width = 
  object(self)
    val pixels_r = Array.make_matrix height width 0.
    val pixels_g = Array.make_matrix height width 0.
    val pixels_b = Array.make_matrix height width 0.
    method pixel (x: int) (y:int) =
      {r= pixels_r.(x).(y); g=pixels_g.(x).(y); b=pixels_b.(x).(y)}
    method height =
      height
    method width =
      width
    method write_pixel (x: int) (y:int) (c: color) =
      if x >= height || y >= width then raise (ValueError "Out of bounds")
      else 
        pixels_r.(x).(y) <- c.r;
        pixels_g.(x).(y) <- c.g;
        pixels_b.(x).(y) <- c.b
    method to_256 (f: float) : int =
      let scaled = int_of_float (f *. 255.) in
      if scaled >= 255 then 255 else if scaled < 0 then 0 else scaled
    method pixel_to_string (x: int) (y: int) : string =
      (* Printf.printf "Pixels %f %f" x y; *)
      let pixel_fmt = format_of_string "%d %d %d " in
      Printf.sprintf pixel_fmt (self#to_256 pixels_r.(x).(y)) (self#to_256 pixels_g.(x).(y)) (self#to_256 pixels_b.(x).(y)) 
    method to_ppm : string =
      (* print_newline (); *)
      (* let () = pixels_r |> Array.iter (Array.iter print_float) in
      let () = pixels_g |> Array.iter (Array.iter print_float) in
      let () = pixels_b |> Array.iter (Array.iter print_float) in *)
      let header_fmt = format_of_string "P3\n%d %d\n255\n" in
      let header = Printf.sprintf header_fmt height width in
      let out = Buffer.create (height * width * 4) in
      Buffer.add_string out header;
      for x = 0 to (height-1) do 
        let overflow_count = ref 0 in
        for y = 0 to (width-1) do
          overflow_count := !overflow_count + 2;
          if !overflow_count >= 60 then Buffer.add_string out "\n";
          if !overflow_count >= 60 then overflow_count := 0;
          (* Printf.printf "%s" (Buffer.contents out); *)
          Buffer.add_string out (self#pixel_to_string x y);
        done;
        Buffer.add_string out "\n";
      done;
      Buffer.contents out
  end