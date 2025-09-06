open Tsdl
open Tsdl_image
open Logging

let min_win_w, min_win_h = (400, 300)

let max_win_w, max_win_h = (ref 0, ref 0)

let owin_w, owin_h = (ref 0, ref 0)

let app_data_path =
  match Scope_sites.Sites.app_data with
  | [] ->
      fatal rc_Error "No app data path provided"
  | h :: _ ->
      h

let init_app () : Sdl.window * Sdl.renderer =
  Sdl.init Sdl.Init.video |> ignore ;
  (* Create window *)
  let w, h = (800, 600) in
  let window =
    Sdl.create_window "Scope Image Viewer" ~x:Sdl.Window.pos_centered
      ~y:Sdl.Window.pos_centered ~w ~h Sdl.Window.shown
    |> Result.get_ok
  in
  owin_w := w ;
  owin_h := h ;
  let mw, mh =
    let r = Sdl.get_display_usable_bounds 0 |> Result.get_ok in
    (Sdl.Rect.w r, Sdl.Rect.h r)
  in
  max_win_w := mw * 99 / 100 ;
  max_win_h := mh * 99 / 100 ;
  Sdl.set_window_resizable window true ;
  Sdl.set_window_minimum_size window ~w:min_win_w ~h:min_win_h ;
  (* Set icon *)
  let icon_path = app_data_path ^ "/logo.png" in
  let surface = Image.load icon_path |> Result.get_ok in
  Sdl.set_window_icon window surface ;
  Sdl.free_surface surface ;
  (* Create renderer *)
  let renderer =
    Sdl.create_renderer window ~index:(-1) ~flags:Sdl.Renderer.accelerated
    |> Result.get_ok
  in
  (* Sdl.set_hint Sdl.Hint.render_scale_quality "best" |> ignore ; *)
  (window, renderer)
