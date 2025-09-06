open Tsdl
open Background
open Formats.Format

type settings =
  { mutable scale: float
  ; mutable offset: int * int
  ; mutable rotation: float
  ; mutable flip: Sdl.flip }

let default_settings (settings : settings) =
  settings.scale <- 1. ;
  settings.offset <- (0, 0) ;
  settings.rotation <- 0. ;
  settings.flip <- Sdl.Flip.none

type image =
  { path: string
  ; mutable format: (format, string) result option
  ; mutable texture: Sdl.texture option }

let min_zoom, max_zoom = (0.01, 100.)

exception InvalidImage

let clear window renderer =
  let win_w, win_h = Sdl.get_window_size window in
  Sdl.render_clear renderer |> ignore ;
  draw_checker_background renderer (get_checker_texture renderer 16) win_w win_h

let compute_fit_scale ?(upscale : bool = false) (img : format) (win_w, win_h) =
  (* Compute scaling factor to fit image within window *)
  let scale_w = float win_w /. float img#width in
  let scale_h = float win_h /. float img#height in
  (* don’t upscale, only shrink *)
  if upscale then
    min scale_w scale_h
  else
    min 1.0 (min scale_w scale_h)

let fit_to_window window (img : Formats.Format.format) (settings : settings) =
  (* Reset offset *)
  settings.offset <- (0, 0) ;
  (* Compute scale to fit the window *)
  let win_w, win_h = Sdl.get_window_size window in
  let scale = compute_fit_scale ~upscale:true img (win_w, win_h) in
  settings.scale <- scale

let draw_texture window renderer (img : Formats.Format.format)
    (texture : Sdl.texture option) (settings : settings) =
  (* If first render (scale = 1.0), compute fit-to-window scale *)
  ( if settings.scale = 1.0 then
      let win_w, win_h = Sdl.get_window_size window in
      let scale = compute_fit_scale img (win_w, win_h) in
      settings.scale <- scale ) ;
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
  Sdl.render_copy_ex renderer ~dst:dst_rect tex settings.rotation None
    settings.flip
  |> ignore ;
  Sdl.render_present renderer ;
  tex
