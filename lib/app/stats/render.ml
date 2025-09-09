open Tsdl
open Histogram
open Misc
open Types

let render_stats (renderer : Sdl.renderer) (surf : Sdl.surface option)
    (img : image) =
  (* Ensure histogram exists *)
  let misc, hist =
    match (img.stats.misc, img.stats.histogram, surf) with
    | Some m, Some h, _ ->
        (Some m, Some h)
    | None, None, Some s ->
        let hist = histogram_of_surface_hex s in
        let h = Some (texture_of_histogram renderer hist) in
        img.stats.histogram <- h ;
        let stats_tex = texture_of_stats renderer img.path hist in
        img.stats.misc <- Some stats_tex ;
        Sdl.set_texture_blend_mode stats_tex Sdl.Blend.mode_blend
        |> Utils.get_sdl_result ;
        (Some stats_tex, h)
    | _, _, _ ->
        (None, None)
  in
  (* --- Render histogram at bottom --- *)
  ( match hist with
  | Some hist_tex ->
      let win_h = !Init.owin_h in
      let _, _, (hist_w, hist_h) =
        Sdl.query_texture hist_tex |> Utils.get_sdl_result
      in
      let dst_rect =
        Sdl.Rect.create ~x:0 ~y:(win_h - hist_h) ~w:hist_w ~h:hist_h
      in
      Sdl.set_texture_blend_mode hist_tex Sdl.Blend.mode_blend
      |> Utils.get_sdl_result ;
      Sdl.set_texture_alpha_mod hist_tex 255 |> Utils.get_sdl_result ;
      Sdl.render_copy renderer ~dst:dst_rect hist_tex |> Utils.get_sdl_result
  | _ ->
      () ) ;
  (* --- Render textual stats at top-left --- *)
  match misc with
  | Some stats_tex ->
      let dst_rect =
        Sdl.Rect.create ~x:0 ~y:0 ~w:(!Init.owin_w / 2) ~h:(!Init.owin_h / 4)
      in
      Sdl.set_texture_alpha_mod stats_tex 255 |> Utils.get_sdl_result ;
      Sdl.render_copy renderer ~dst:dst_rect stats_tex |> Utils.get_sdl_result
  | _ ->
      ()
