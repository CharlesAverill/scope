open Tsdl
open Formats.Manager
open Logging

let init_app () : Sdl.window * Sdl.renderer =
  Sdl.init Sdl.Init.video |> ignore ;
  let window =
    Sdl.create_window "Scope Image Viewer" ~x:Sdl.Window.pos_centered
      ~y:Sdl.Window.pos_centered ~w:800 ~h:600 Sdl.Window.shown
    |> Result.get_ok
  in
  Sdl.set_window_minimum_size window ~w:400 ~h:300 |> ignore ;
  let renderer =
    Sdl.create_renderer window ~index:(-1) ~flags:Sdl.Renderer.accelerated
    |> Result.get_ok
  in
  (window, renderer)

let draw_texture (window : Sdl.window) (renderer : Sdl.renderer)
    (img : Formats.Format.format) (texture : Sdl.texture option) =
  (* get current window size *)
  let win_w, win_h = Sdl.get_window_size window in
  let win_w, win_h = (max img#width win_w, max img#height win_h) in
  Sdl.set_window_size window ~w:win_w ~h:win_h ;
  (* calculate top-left corner to center the image *)
  let dst_x = (win_w - img#width) / 2 in
  let dst_y = (win_h - img#height) / 2 in
  let dst_rect = Sdl.Rect.create ~x:dst_x ~y:dst_y ~w:img#width ~h:img#height in
  Sdl.set_window_title window (img#filename ^ " - Scope Image File Viewer") ;
  Printf.printf "%s %d %d\n" img#filename img#width img#height ;
  flush stdout ;
  let tex =
    match texture with
    | Some tex ->
        tex
    | None -> (
      match img#to_surf with
      | Some surf ->
          print_endline "surface created" ;
          let tex =
            Sdl.create_texture_from_surface renderer surf |> Result.get_ok
          in
          Sdl.free_surface surf ; tex
      | None ->
          fatal rc_Error "Couldn't generate SDL surface" )
  in
  Sdl.render_clear renderer |> ignore ;
  print_endline "drawing" ;
  Sdl.render_copy renderer ~dst:dst_rect tex |> ignore ;
  Sdl.render_present renderer ;
  tex

let main_loop window renderer (image_paths : string list) : unit =
  if image_paths = [] then
    ()
  else
    let n = List.length image_paths in
    let loaded_imgs = Array.make n None in
    let loaded_textures = Array.make n None in
    let idx = ref 0 in
    let draw_at i =
      Sdl.render_clear renderer |> ignore ;
      Sdl.render_present renderer ;
      match loaded_imgs.(i) with
      | Some (Ok img) ->
          draw_texture window renderer img loaded_textures.(i) |> ignore
      | Some (Error s) ->
          Printf.printf "Error: %s\n" s ;
          exit 1
      | None -> (
        match format_image (List.nth image_paths i) with
        | Ok img ->
            loaded_imgs.(i) <- Some (Ok img) ;
            loaded_textures.(i) <- Some (draw_texture window renderer img None)
        | Error s ->
            Printf.printf "Error: %s\n" s ;
            exit 1 )
    in
    draw_at !idx ;
    let break = ref false in
    let event = Sdl.Event.create () in
    while not !break do
      if Sdl.poll_event (Some event) then
        match Sdl.Event.(get event typ) with
        | t when t = Sdl.Event.quit ->
            break := true
        | t when t = Sdl.Event.key_down -> (
          match Sdl.Event.(get event Sdl.Event.keyboard_keycode) with
          | k when k = Sdl.K.right ->
              if !idx < n - 1 then
                idx := !idx + 1
              else
                idx := 0 ;
              draw_at !idx
          | k when k = Sdl.K.left ->
              if !idx > 0 then
                idx := !idx - 1
              else
                idx := n - 1 ;
              draw_at !idx
          | k when k = Sdl.K.escape ->
              break := true
          | _ ->
              () )
        | _ ->
            ()
    done ;
    Sdl.quit () ;
    exit 0
