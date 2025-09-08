open Tsdl
open Background
open Formats.Format
open Types

let min_zoom, max_zoom = (0.01, 100.)

exception InvalidImage

let clear renderer =
  Sdl.render_clear renderer |> Utils.get_sdl_result ;
  draw_checker_background renderer
    (get_checker_texture renderer 16)
    !Init.owin_w !Init.owin_h

let compute_fit_scale ?(upscale : bool = false) (fmt : format) (win_w, win_h) =
  (* Compute scaling factor to fit image within window *)
  let scale_w = float win_w /. float fmt#width in
  let scale_h = float win_h /. float fmt#height in
  (* don’t upscale, only shrink *)
  if upscale then
    min scale_w scale_h
  else
    min 1.0 (min scale_w scale_h)

let fit_to_window (fmt : Formats.Format.format) (settings : settings) =
  (* Reset offset *)
  settings.offset <- (0, 0) ;
  (* Compute scale to fit the window *)
  let win_w, win_h = (!Init.owin_w, !Init.owin_h) in
  let scale = compute_fit_scale ~upscale:true fmt (win_w, win_h) in
  settings.scale <- scale

let draw_texture window renderer (fmt : Formats.Format.format) (img : image)
    (texture : Sdl.texture option) (settings : settings) =
  (* If first render (scale = 1.0), compute fit-to-window scale *)
  ( if settings.scale = 1.0 then
      let win_w, win_h = (!Init.owin_w, !Init.owin_h) in
      let scale = compute_fit_scale fmt (win_w, win_h) in
      settings.scale <- scale ) ;
  let scaled_w = int_of_float (float fmt#width *. settings.scale) in
  let scaled_h = int_of_float (float fmt#height *. settings.scale) in
  let win_w, win_h = Sdl.get_window_size window in
  (* Center the image *)
  let dst_x = ((win_w - scaled_w) / 2) + fst settings.offset in
  let dst_y = ((win_h - scaled_h) / 2) + snd settings.offset in
  let dst_rect = Sdl.Rect.create ~x:dst_x ~y:dst_y ~w:scaled_w ~h:scaled_h in
  let surf, tex =
    match texture with
    | Some tex ->
        (None, tex)
    | None -> (
      match fmt#to_surf with
      | Some surf ->
          let tex =
            Sdl.create_texture_from_surface renderer surf
            |> Utils.get_sdl_result
          in
          (Some surf, tex)
      | None ->
          raise InvalidImage )
  in
  clear renderer ;
  Sdl.render_copy_ex renderer ~dst:dst_rect tex settings.rotation None
    settings.flip
  |> Utils.get_sdl_result ;
  if settings.render_stats then
    Stats.Render.render_stats window renderer surf img ;
  Sdl.render_present renderer ;
  (match surf with None -> () | Some s -> Sdl.free_surface s) ;
  tex
