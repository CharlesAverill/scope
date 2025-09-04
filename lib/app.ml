open Tsdl
open Tsdl_image
open Formats.Manager
open Logging

let min_win_w, min_win_h = (400, 300)

let owin_w, owin_h = (ref 0, ref 0)

let app_data_path =
  match Scope_sites.Sites.app_data with
  | [] ->
      fatal rc_Error "No app data path provided"
  | h :: _ ->
      h

let init_app () : Sdl.window * Sdl.renderer =
  Sdl.init Sdl.Init.video |> ignore ;
  let window =
    Sdl.create_window "Scope Image Viewer" ~x:Sdl.Window.pos_centered
      ~y:Sdl.Window.pos_centered ~w:800 ~h:600 Sdl.Window.shown
    |> Result.get_ok
  in
  owin_w := 800 ;
  owin_h := 600 ;
  Sdl.set_window_resizable window true ;
  Sdl.set_window_minimum_size window ~w:min_win_w ~h:min_win_h |> ignore ;
  let icon_path = app_data_path ^ "/logo.png" in
  let surface = Image.load icon_path |> Result.get_ok in
  Sdl.set_window_icon window surface ;
  Sdl.free_surface surface ;
  let renderer =
    Sdl.create_renderer window ~index:(-1) ~flags:Sdl.Renderer.accelerated
    |> Result.get_ok
  in
  (window, renderer)

type settings = {mutable scale: float; mutable offset: int * int}

let min_zoom, max_zoom = (0.1, 100.)

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
              Int32.of_string "0xFFFFFFFF" (* white *)
            else
              Int32.of_string "0xFFCCCCCC" (* light gray *)
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

let clear window renderer =
  let win_w, win_h = Sdl.get_window_size window in
  Sdl.render_clear renderer |> ignore ;
  draw_checker_background renderer (get_checker_texture renderer 16) win_w win_h

exception InvalidImage

let draw_texture (window : Sdl.window) (renderer : Sdl.renderer)
    (img : Formats.Format.format) (texture : Sdl.texture option)
    (settings : settings) =
  (* compute scaled dimensions *)
  let scaled_w = int_of_float (float img#width *. settings.scale) in
  let scaled_h = int_of_float (float img#height *. settings.scale) in
  (* get current window size *)
  let win_w, win_h = (max img#width min_win_w, max img#height min_win_h) in
  if win_w <> !owin_w || win_h <> !owin_h then (
    Sdl.set_window_size window ~w:win_w ~h:win_h ;
    owin_w := win_w ;
    owin_h := win_h
  ) ;
  (* calculate top-left corner to center the image *)
  let dst_x = ((win_w - scaled_w) / 2) + fst settings.offset in
  let dst_y = ((win_h - scaled_h) / 2) + snd settings.offset in
  let dst_rect = Sdl.Rect.create ~x:dst_x ~y:dst_y ~w:scaled_w ~h:scaled_h in
  _log Log_Debug "%s %d %d (scaled %.2f)\n" img#filename img#width img#height
    settings.scale ;
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

let is_plus event =
  let key = Sdl.Event.(get event keyboard_keycode) in
  let mods = Sdl.get_mod_state () in
  (key = Sdl.K.equals && mods land Sdl.Kmod.shift <> 0) || key = Sdl.K.kp_plus

let default_settings settings =
  settings.scale <- 1. ;
  settings.offset <- (0, 0)

let quit () = Sdl.quit () ; exit 0

let remove_from_array (arr : 'a array ref) (idx : int) : unit =
  let n = Array.length !arr in
  if idx < 0 || idx >= n then
    invalid_arg "remove_from_array: index out of bounds" ;
  arr :=
    Array.init (n - 1) (fun i ->
        if i < idx then
          !arr.(i)
        else
          !arr.(i + 1) )

let main_loop window renderer (image_paths : string list) : unit =
  let settings = {scale= 1.; offset= (0, 0)} in
  if image_paths = [] then
    ()
  else
    let image_paths = ref (Array.of_list image_paths) in
    let n = Array.length !image_paths in
    let loaded_imgs = ref (Array.make n None) in
    let loaded_textures = ref (Array.make n None) in
    let idx = ref 0 in
    let rec draw_at ?(present_after_clear = true) ?(update_name : bool = false)
        i =
      if
        Array.for_all
          (fun i -> match i with Some (Error _) -> true | _ -> false)
          !loaded_imgs
      then
        quit ()
      else (
        clear window renderer ;
        if present_after_clear then Sdl.render_present renderer ;
        match !loaded_imgs.(i) with
        | Some (Ok img) ->
            if update_name then
              Sdl.set_window_title window
                (!image_paths.(i) ^ " - Scope Image File Viewer") ;
            draw_texture window renderer img !loaded_textures.(i) settings
            |> ignore
        | Some (Error _) ->
            remove_from_array loaded_imgs i ;
            remove_from_array loaded_textures i ;
            remove_from_array image_paths i ;
            draw_at ~update_name ~present_after_clear i
        | None -> (
          match format_image !image_paths.(i) with
          | Ok img -> (
              if update_name then
                Sdl.set_window_title window
                  (!image_paths.(i) ^ " - Scope Image File Viewer") ;
              try
                let tex =
                  Some (draw_texture window renderer img None settings)
                in
                !loaded_imgs.(i) <- Some (Ok img) ;
                !loaded_textures.(i) <- tex
              with InvalidImage ->
                remove_from_array image_paths i ;
                remove_from_array loaded_imgs i ;
                remove_from_array loaded_textures i ;
                draw_at ~present_after_clear ~update_name i )
          | Error s ->
              _log Log_Error "Skipping invalid image '%s': %s\n"
                !image_paths.(i) s ;
              remove_from_array image_paths i ;
              remove_from_array loaded_imgs i ;
              remove_from_array loaded_textures i ;
              draw_at ~present_after_clear ~update_name i )
      )
    in
    draw_at ~present_after_clear:true ~update_name:true !idx ;
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
                draw_at ~update_name:true !idx
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
                draw_at ~update_name:true !idx
              )
          | k when k = Sdl.K.escape ->
              break := true
          | _ when is_plus event ->
              settings.scale <- min max_zoom (settings.scale *. 1.25) ;
              draw_at !idx
          | k when k = Sdl.K.minus || k = Sdl.K.kp_minus ->
              settings.scale <- max min_zoom (settings.scale /. 1.25) ;
              draw_at !idx
          | _ ->
              () )
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
    quit ()
