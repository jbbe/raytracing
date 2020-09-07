open Color

exception ValueError of string
class canvas width height= 
  object(self)
    val pixels_r = Array.make_matrix height width 0.
    val pixels_g = Array.make_matrix height width 0.
    val pixels_b = Array.make_matrix height width 0.
    method pixel (x: int) (y:int) =
      {r= pixels_r.(y).(x); g=pixels_g.(y).(x); b=pixels_b.(y).(x)}
    method height =
      height
    method width =
      width
    method write_pixel (x: int) (y:int) (c: color) =
      if x >= width || y >= height then raise (ValueError "Out of bounds")
      else 
        pixels_r.(y).(x) <- c.r;
        pixels_g.(y).(x) <- c.g;
        pixels_b.(y).(x) <- c.b
    method to_256 (f: float) : int =
      let scaled = int_of_float (f *. 255.) in
      if scaled >= 255 then 255 else if scaled < 0 then 0 else scaled
    method pixel_to_string (x: int) (y: int) : string =
      (* Printf.printf "Pixels %f %f" x y; *)
      let pixel_fmt = format_of_string "%d %d %d " in
      Printf.sprintf pixel_fmt (self#to_256 pixels_r.(y).(x)) (self#to_256 pixels_g.(y).(x)) (self#to_256 pixels_b.(y).(x)) 
    method to_ppm : string =
      (* print_newline (); *)
      (* let () = pixels_r |> Array.iter (Array.iter print_float) in
      let () = pixels_g |> Array.iter (Array.iter print_float) in
      let () = pixels_b |> Array.iter (Array.iter print_float) in *)
      let header_fmt = format_of_string "P3\n%d %d\n255\n" in
      let header = Printf.sprintf header_fmt width height in
      let out = Buffer.create (height * width * 4) in
      Buffer.add_string out header;
      for x = 0 to (width-1) do 
        (* Printf.printf "Buf Size %d %d\n" (Buffer.length out) x; *)
        let overflow_count = ref 0 in
        for y = 0 to (height-1) do
          overflow_count := !overflow_count + 3;
          if !overflow_count >= 60 then Buffer.add_string out "\n";
          if !overflow_count >= 60 then overflow_count := 0;
          (* Printf.printf "%s" (Buffer.contents out); *)
          Buffer.add_string out (self#pixel_to_string x y);
        done;
        Buffer.add_string out "\n";
      done;
      (* Buffer.add_string out "\n"; *)
      (* Printf.printf "Buf Size %d" (Buffer.length out); *)

      (* Printf.printf "%s" (Buffer.contents out); *)
      Buffer.contents out
  end
