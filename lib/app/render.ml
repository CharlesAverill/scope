open Tsdl
open Background
open Init
open Formats.Format

type settings = {mutable scale: float; mutable offset: int * int}

let min_zoom, max_zoom = (0.1, 100.)

exception InvalidImage

let clear window renderer =
  let win_w, win_h = Sdl.get_window_size window in
  Sdl.render_clear renderer |> ignore ;
  draw_checker_background renderer (get_checker_texture renderer 16) win_w win_h

let compute_initial_window_and_scale (img : format) =
  (* Determine scaling factor to fit image in max window size *)
  let scale_w = float !max_win_w /. float img#width in
  let scale_h = float !max_win_h /. float img#height in
  let scale = min 1.0 (min scale_w scale_h) in
  (* never upscale *)
  (* Compute window size for initial render *)
  let win_w = int_of_float (float img#width *. scale) in
  let win_h = int_of_float (float img#height *. scale) in
  (win_w, win_h, scale)

let draw_texture window renderer (img : Formats.Format.format)
    (texture : Sdl.texture option) (settings : settings) =
  (* On first render, compute scale and window size if scale = 1 *)
  if settings.scale = 1.0 then (
    let win_w, win_h, scale = compute_initial_window_and_scale img in
    settings.scale <- scale ;
    Printf.printf "%s %f\n" img#filename settings.scale ;
    flush stdout ;
    (* if scale < 1.0 then *)
    (* Sdl.render_set_logical_size renderer win_w win_h |> ignore ; *)
    Sdl.set_window_size window ~w:win_w ~h:win_h |> ignore ;
    Init.owin_w := win_w ;
    Init.owin_h := win_h
  ) ;
  let scaled_w = int_of_float (float img#width *. settings.scale) in
  let scaled_h = int_of_float (float img#height *. settings.scale) in
  let win_w, win_h = Sdl.get_window_size window in
  (* Center the image *)
  let dst_x = ((win_w - scaled_w) / 2) + fst settings.offset in
  let dst_y = ((win_h - scaled_h) / 2) + snd settings.offset in
  let dst_rect = Sdl.Rect.create ~x:dst_x ~y:dst_y ~w:scaled_w ~h:scaled_h in
  let tex =
    match texture with
    | Some tex ->
        tex
    | None -> (
      match img#to_surf with
      | Some surf ->
          let tex =
            Sdl.create_texture_from_surface renderer surf |> Result.get_ok
          in
          Sdl.free_surface surf ; tex
      | None ->
          raise InvalidImage )
  in
  clear window renderer ;
  Sdl.render_copy renderer ~dst:dst_rect tex |> ignore ;
  Sdl.render_present renderer ;
  tex
