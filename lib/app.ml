open Tsdl
open Formats.Manager
open Logging

let min_win_w, min_win_h = (400, 300)

let init_app () : Sdl.window * Sdl.renderer =
  Sdl.init Sdl.Init.video |> ignore ;
  let window =
    Sdl.create_window "Scope Image Viewer" ~x:Sdl.Window.pos_centered
      ~y:Sdl.Window.pos_centered ~w:800 ~h:600 Sdl.Window.shown
    |> Result.get_ok
  in
  Sdl.set_window_resizable window true ;
  Sdl.set_window_minimum_size window ~w:min_win_w ~h:min_win_h |> ignore ;
  let renderer =
    Sdl.create_renderer window ~index:(-1) ~flags:Sdl.Renderer.accelerated
    |> Result.get_ok
  in
  (window, renderer)

type settings = {mutable scale: float; mutable offset: int * int}

let min_zoom, max_zoom = (0.1, 5.)

let create_checker_texture renderer tile_size =
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
          Int32.of_string "0xFFFFFFFF" (* white *)
        else
          Int32.of_string "0xFFCCCCCC" (* light gray *)
      in
      Bigarray.Array1.set pixels ((y * tile_size) + x) color
    done
  done ;
  let tex = Sdl.create_texture_from_surface renderer surf |> Result.get_ok in
  Sdl.free_surface surf ; tex

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
  loop_y 0 ;
  Sdl.destroy_texture checker_tex

let clear window renderer =
  let win_w, win_h = Sdl.get_window_size window in
  Sdl.render_clear renderer |> ignore ;
  draw_checker_background renderer
    (create_checker_texture renderer 16)
    win_w win_h

let draw_texture (window : Sdl.window) (renderer : Sdl.renderer)
    (img : Formats.Format.format) (texture : Sdl.texture option)
    (settings : settings) =
  (* compute scaled dimensions *)
  let scaled_w = int_of_float (float img#width *. settings.scale) in
  let scaled_h = int_of_float (float img#height *. settings.scale) in
  (* get current window size *)
  let win_w, win_h = Sdl.get_window_size window in
  let win_w, win_h = (max img#width min_win_w, max img#height min_win_h) in
  Sdl.set_window_size window ~w:win_w ~h:win_h ;
  (* calculate top-left corner to center the image *)
  let dst_x = ((win_w - scaled_w) / 2) + fst settings.offset in
  let dst_y = ((win_h - scaled_h) / 2) + snd settings.offset in
  let dst_rect = Sdl.Rect.create ~x:dst_x ~y:dst_y ~w:scaled_w ~h:scaled_h in
  Sdl.set_window_title window (img#filename ^ " - Scope Image File Viewer") ;
  Printf.printf "%s %d %d (scaled %.2f)\n" img#filename img#width img#height
    settings.scale ;
  flush stdout ;
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
          fatal rc_Error "Couldn't generate SDL surface" )
  in
  clear window renderer ;
  Sdl.render_copy renderer ~dst:dst_rect tex |> ignore ;
  Sdl.render_present renderer ;
  tex

let is_plus event =
  let key = Sdl.Event.(get event keyboard_keycode) in
  let mods = Sdl.get_mod_state () in
  (key = Sdl.K.equals && mods land Sdl.Kmod.shift <> 0) || key = Sdl.K.kp_plus

let default_settings settings =
  settings.scale <- 1. ;
  settings.offset <- (0, 0)

let main_loop window renderer (image_paths : string list) : unit =
  let settings = {scale= 1.; offset= (0, 0)} in
  if image_paths = [] then
    ()
  else
    let n = List.length image_paths in
    let loaded_imgs = Array.make n None in
    let loaded_textures = Array.make n None in
    let idx = ref 0 in
    let draw_at ?(present_after_clear : bool = true) i =
      clear window renderer ;
      if present_after_clear then Sdl.render_present renderer ;
      match loaded_imgs.(i) with
      | Some (Ok img) ->
          draw_texture window renderer img loaded_textures.(i) settings
          |> ignore
      | Some (Error s) ->
          Printf.printf "Error: %s\n" s ;
          exit 1
      | None -> (
        match format_image (List.nth image_paths i) with
        | Ok img ->
            loaded_imgs.(i) <- Some (Ok img) ;
            loaded_textures.(i) <-
              Some (draw_texture window renderer img None settings)
        | Error s ->
            Printf.printf "Error: %s\n" s ;
            exit 1 )
    in
    draw_at !idx ;
    let break = ref false in
    let event = Sdl.Event.create () in
    let dragging = ref false in
    let last_mouse_pos = ref (0, 0) in
    while not !break do
      if Sdl.poll_event (Some event) then
        match Sdl.Event.(get event typ) with
        | t when t = Sdl.Event.quit ->
            break := true
        | t when t = Sdl.Event.key_down -> (
          match Sdl.Event.(get event Sdl.Event.keyboard_keycode) with
          | k when k = Sdl.K.right || k = Sdl.K.down ->
              let new_idx =
                if !idx < n - 1 then
                  !idx + 1
                else
                  0
              in
              if new_idx <> !idx then (
                idx := new_idx ;
                default_settings settings ;
                draw_at !idx
              )
          | k when k = Sdl.K.left || k = Sdl.K.up ->
              let new_idx =
                if !idx > 0 then
                  !idx - 1
                else
                  n - 1
              in
              if new_idx <> !idx then (
                idx := new_idx ;
                default_settings settings ;
                draw_at !idx
              )
          | k when k = Sdl.K.escape ->
              break := true
          | k when is_plus event ->
              settings.scale <- min max_zoom (settings.scale *. 1.25) ;
              draw_at !idx
          | k when k = Sdl.K.minus || k = Sdl.K.kp_minus ->
              settings.scale <- max min_zoom (settings.scale /. 1.25) ;
              draw_at !idx
          | k ->
              Printf.printf "keypress: %d\n" k )
        | t when t = Sdl.Event.mouse_button_down ->
            let x = Sdl.Event.(get event mouse_button_x)
            and y = Sdl.Event.(get event mouse_button_y) in
            if Sdl.Event.(get event mouse_button_button) = Sdl.Button.left then (
              dragging := true ;
              last_mouse_pos := (x, y)
            )
        | t when t = Sdl.Event.mouse_button_up ->
            if Sdl.Event.(get event mouse_button_button) = Sdl.Button.left then
              dragging := false
        | t when t = Sdl.Event.mouse_motion ->
            if !dragging then (
              let x = Sdl.Event.(get event mouse_motion_x)
              and y = Sdl.Event.(get event mouse_motion_y) in
              let lx, ly = !last_mouse_pos in
              let dx = x - lx and dy = y - ly in
              let ox, oy = settings.offset in
              settings.offset <- (ox + dx, oy + dy) ;
              last_mouse_pos := (x, y) ;
              draw_at ~present_after_clear:false !idx
            )
        | _ ->
            ()
    done ;
    Sdl.quit () ;
    exit 0
