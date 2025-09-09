open Tsdl
open Background
open Types

exception InvalidImage

let clear renderer =
  Sdl.render_clear renderer |> Utils.get_sdl_result ;
  draw_checker_background renderer
    (get_checker_texture renderer 16)
    !Init.owin_w !Init.owin_h

let draw_texture window renderer (fmt : Formats.Format.format) (img : image)
    (texture : Sdl.texture option) =
  (* If first render (scale = 1.0), compute fit-to-window scale *)
  update_settings (fun settings ->
      if settings.scale = 1.0 then
        let win_w, win_h = (!Init.owin_w, !Init.owin_h) in
        let scale = Events.compute_fit_scale fmt (win_w, win_h) in
        settings.scale <- scale ) ;
  let scale, offset, rot, flip, render_stats =
    get_settings (fun s ->
        (s.scale, s.offset, s.rotation, s.flip, s.render_stats) )
  in
  let scaled_w = int_of_float (float fmt#width *. scale) in
  let scaled_h = int_of_float (float fmt#height *. scale) in
  let win_w, win_h = Sdl.get_window_size window in
  (* Center the image *)
  let dst_x = ((win_w - scaled_w) / 2) + fst offset in
  let dst_y = ((win_h - scaled_h) / 2) + snd offset in
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
  Sdl.render_copy_ex renderer ~dst:dst_rect tex rot None flip
  |> Utils.get_sdl_result ;
  if render_stats then Stats.Render.render_stats renderer surf img ;
  Sdl.render_present renderer ;
  (match surf with None -> () | Some s -> Sdl.free_surface s) ;
  tex
