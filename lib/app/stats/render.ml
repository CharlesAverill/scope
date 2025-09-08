open Tsdl
open Histogram
open Misc
open Types

let render_stats (window : Sdl.window) (renderer : Sdl.renderer)
    (surf : Sdl.surface option) (img : image) =
  (* Ensure histogram exists *)
  let hist =
    match (img.stats.histogram, surf) with
    | Some h, _ ->
        h
    | None, Some s ->
        let h = histogram_of_surface_hex s in
        img.stats.histogram <- Some h ;
        h
    | None, None ->
        ColorMap.empty
  in
  (* --- Render histogram at bottom --- *)
  if ColorMap.cardinal hist > 0 then (
    let hist_tex = texture_of_histogram renderer hist in
    let _, win_h = Sdl.get_window_size window in
    let _, _, (hist_w, hist_h) =
      Sdl.query_texture hist_tex |> Utils.get_sdl_result
    in
    let dst_rect =
      Sdl.Rect.create ~x:0 ~y:(win_h - hist_h) ~w:hist_w ~h:hist_h
    in
    Sdl.set_texture_blend_mode hist_tex Sdl.Blend.mode_blend
    |> Utils.get_sdl_result ;
    Sdl.set_texture_alpha_mod hist_tex 255 |> Utils.get_sdl_result ;
    Sdl.render_copy renderer ~dst:dst_rect hist_tex |> Utils.get_sdl_result ;
    Sdl.destroy_texture hist_tex
  ) ;
  (* --- Render textual stats at top-left --- *)
  let stats_tex = texture_of_stats renderer img.path hist in
  let dst_rect =
    Sdl.Rect.create ~x:0 ~y:0 ~w:(!Init.owin_w / 2) ~h:(!Init.owin_h / 4)
  in
  Sdl.set_texture_blend_mode stats_tex Sdl.Blend.mode_blend
  |> Utils.get_sdl_result ;
  Sdl.set_texture_alpha_mod stats_tex 255 |> Utils.get_sdl_result ;
  Sdl.render_copy renderer ~dst:dst_rect stats_tex |> Utils.get_sdl_result ;
  Sdl.destroy_texture stats_tex
