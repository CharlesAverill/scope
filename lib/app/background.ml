open Tsdl

let checker_tex = ref None

let get_checker_texture renderer tile_size =
  match !checker_tex with
  | Some t ->
      t
  | None ->
      let surf =
        Sdl.create_rgb_surface_with_format ~w:tile_size ~h:tile_size ~depth:32
          Sdl.Pixel.format_argb8888
        |> Result.get_ok
      in
      let pixels = Sdl.get_surface_pixels surf Bigarray.int32 in
      for y = 0 to tile_size - 1 do
        for x = 0 to tile_size - 1 do
          let is_white =
            ((x / (tile_size / 2)) + (y / (tile_size / 2))) mod 2 = 0
          in
          let color =
            if is_white then
              Int32.of_string "0xFFFFFFFF"
            else
              Int32.of_string "0xFFCCCCCC"
          in
          Bigarray.Array1.set pixels ((y * tile_size) + x) color
        done
      done ;
      let tex =
        Sdl.create_texture_from_surface renderer surf |> Result.get_ok
      in
      Sdl.free_surface surf ;
      checker_tex := Some tex ;
      tex

let draw_checker_background renderer checker_tex win_w win_h =
  let _, _, (tw, th) = Sdl.query_texture checker_tex |> Result.get_ok in
  let rec loop_y y =
    if y < win_h then (
      let rec loop_x x =
        if x < win_w then (
          let dst = Sdl.Rect.create ~x ~y ~w:tw ~h:th in
          Sdl.render_copy renderer ~dst checker_tex |> ignore ;
          loop_x (x + tw)
        )
      in
      loop_x 0 ;
      loop_y (y + th)
    )
  in
  loop_y 0
