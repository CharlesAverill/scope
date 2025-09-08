open Tsdl
open Render
open Types

type t_draw_at =
  ?present_after_clear:bool -> ?new_render:bool -> ?fit:bool -> int -> int

let is_plus event =
  let key = Sdl.Event.(get event keyboard_keycode) in
  let mods = Sdl.get_mod_state () in
  (key = Sdl.K.equals && mods land Sdl.Kmod.shift <> 0) || key = Sdl.K.kp_plus

let ctrl_held () = Sdl.get_mod_state () land Sdl.Kmod.ctrl <> 0

let shift_held () = Sdl.get_mod_state () land Sdl.Kmod.shift <> 0

let mouse_wheel_direction event =
  if Sdl.Event.(get event typ) = Sdl.Event.mouse_wheel then
    let y = Sdl.Event.(get event mouse_wheel_y) in
    if y > 0 then
      Some `Up
    else if y < 0 then
      Some `Down
    else
      None
  else
    None

let handle_window_event (draw_at : t_draw_at) window (settings : settings)
    (state : state) = function
  | we when we = Sdl.Event.window_event_resized ->
      (* Re-center and re-draw *)
      settings.offset <- (0, 0) ;
      let w, h = Sdl.get_window_size window in
      Init.owin_w := w ;
      Init.owin_h := h ;
      state.draw_idx <- draw_at state.draw_idx
  | _ ->
      ()

let flip_horizontal settings =
  Sdl.Flip.(
    settings.flip <-
      ( if settings.flip = none then
          horizontal
        else if settings.flip = horizontal then
          none
        else if settings.flip = vertical then
          vertical + horizontal
        else
          vertical ) )

let flip_vertical settings =
  Sdl.Flip.(
    settings.flip <-
      ( if settings.flip = none then
          vertical
        else if settings.flip = horizontal then
          horizontal + vertical
        else if settings.flip = vertical then
          none
        else
          horizontal ) )

let toggle_fullscreen window =
  Sdl.Window.(
    let flags = Sdl.get_window_flags window in
    let is_fullscreen = test flags fullscreen_desktop in
    let new_flag =
      if is_fullscreen then
        windowed
      else
        fullscreen_desktop
    in
    Sdl.set_window_fullscreen window new_flag |> Utils.get_sdl_result )

let img_array image_paths =
  Array.init (List.length image_paths) (fun i ->
      { path= List.nth image_paths i
      ; format= None
      ; texture= None
      ; stats= {histogram= None} } )

let handle_key_down_event (draw_at : t_draw_at) window n settings state =
  (* Keycode *)
  match Sdl.Event.(get state.event Sdl.Event.keyboard_keycode) with
  | k when k = Sdl.K.right || k = Sdl.K.down ->
      (* next image *)
      let new_idx =
        if state.draw_idx < n () - 1 then
          state.draw_idx + 1
        else
          0
      in
      if new_idx <> state.draw_idx then (
        default_settings settings ;
        state.draw_idx <- draw_at ~new_render:true new_idx
      )
  | k when k = Sdl.K.left || k = Sdl.K.up ->
      (* previous image *)
      let new_idx =
        if state.draw_idx > 0 then
          state.draw_idx - 1
        else
          n () - 1
      in
      if new_idx <> state.draw_idx then (
        default_settings settings ;
        state.draw_idx <- draw_at ~new_render:true new_idx
      )
  | k when k = Sdl.K.home ->
      (* go to first image *)
      let new_idx = 0 in
      if new_idx <> state.draw_idx then (
        default_settings settings ;
        state.draw_idx <- draw_at ~new_render:true new_idx
      )
  | k when k = Sdl.K.kend ->
      (* go to last image *)
      let new_idx = n () - 1 in
      if new_idx <> state.draw_idx then (
        default_settings settings ;
        state.draw_idx <- draw_at ~new_render:true new_idx
      )
  | k when k = Sdl.K.s ->
      (* render stats *)
      settings.render_stats <- not settings.render_stats ;
      state.imgs.(state.draw_idx).texture <- None ;
      state.draw_idx <- draw_at state.draw_idx
  | k when k = Sdl.K.escape || (k = Sdl.K.w && ctrl_held ()) ->
      (* exit *)
      state.break <- true
  | _ when is_plus state.event ->
      (* zoom in *)
      settings.scale <- min max_zoom (settings.scale *. 1.25) ;
      state.draw_idx <- draw_at ~present_after_clear:false state.draw_idx
  | k when k = Sdl.K.minus || k = Sdl.K.kp_minus ->
      (* zoom out *)
      settings.scale <- max min_zoom (settings.scale /. 1.25) ;
      state.draw_idx <- draw_at ~present_after_clear:false state.draw_idx
  | k when k = Sdl.K.r && not (ctrl_held ()) ->
      (* reset view *)
      default_settings settings
  | k when k = Sdl.K.r && ctrl_held () ->
      (* reload image view *)
      default_settings settings ;
      state.imgs.(state.draw_idx) <-
        { path= state.imgs.(state.draw_idx).path
        ; format= None
        ; texture= None
        ; stats= {histogram= None} } ;
      state.draw_idx <- draw_at ~new_render:true state.draw_idx
  | k when k = Sdl.K.f ->
      (* fit to window *)
      state.draw_idx <- draw_at ~fit:true state.draw_idx
  | k when k = Sdl.K.h ->
      (* flip horizontally *)
      flip_horizontal settings ;
      state.draw_idx <- draw_at state.draw_idx
  | k when k = Sdl.K.v ->
      (* flip vertically *)
      flip_vertical settings ;
      state.draw_idx <- draw_at state.draw_idx
  | k when k = Sdl.K.f11 ->
      (* toggle fullscreen *)
      toggle_fullscreen window ;
      state.draw_idx <- draw_at ~present_after_clear:false state.draw_idx
  | k when k = Sdl.K.o && ctrl_held () -> (
      let
      (* open file *)
      open
        Tinyfiledialogs in
      match
        open_file_dialog ~title:"Select Image" ~default_path:""
          ~filter_patterns:
            (Some (List.map (( ^ ) "*") Formats.Manager.supported_filetypes))
          ~filter_desc:"Image File" ~allow_multiple:true
      with
      | None ->
          ()
      | Some files -> (
        match
          List.filter
            (fun path ->
              not
                (Array.exists
                   (fun img -> Utils.same_file img.path path)
                   state.imgs ) )
            (String.split_on_char '|' files)
        with
        | [] ->
            ()
        | files ->
            state.draw_idx <- n () ;
            state.imgs <- Array.append state.imgs (img_array files) ;
            default_settings settings ;
            state.draw_idx <- draw_at ~new_render:true state.draw_idx ) )
  | _ ->
      ()

let handle_mouse_button_down_event state =
  let x = Sdl.Event.(get state.event mouse_button_x)
  and y = Sdl.Event.(get state.event mouse_button_y) in
  if Sdl.Event.(get state.event mouse_button_button) = Sdl.Button.left then (
    state.dragging <- true ;
    state.last_mouse_pos <- (x, y)
  )

let handle_mouse_button_up_event state =
  if Sdl.Event.(get state.event mouse_button_button) = Sdl.Button.left then
    state.dragging <- false

let handle_mouse_motion_event (draw_at : t_draw_at) settings state =
  if state.dragging then (
    let x = Sdl.Event.(get state.event mouse_motion_x)
    and y = Sdl.Event.(get state.event mouse_motion_y) in
    let lx, ly = state.last_mouse_pos in
    let dx = x - lx and dy = y - ly in
    let ox, oy = settings.offset in
    settings.offset <- (ox + dx, oy + dy) ;
    state.last_mouse_pos <- (x, y) ;
    draw_at ~present_after_clear:false state.draw_idx |> ignore
  )

let handle_mouse_wheel_event (draw_at : t_draw_at) (settings : settings)
    (state : state) direction =
  settings.scale <-
    max min_zoom
      (let f = match direction with `Up -> Float.mul | `Down -> Float.div in
       f settings.scale 1.25 ) ;
  draw_at ~present_after_clear:false state.draw_idx |> ignore

let handle_event (draw_at : t_draw_at) window n (state : state)
    (settings : settings) = function
  | t when t = Sdl.Event.quit ->
      state.break <- true
  | t when t = Sdl.Event.window_event ->
      handle_window_event draw_at window settings state
        Sdl.Event.(get state.event window_event_id)
  | t when t = Sdl.Event.key_down ->
      handle_key_down_event draw_at window n settings state
  | t when t = Sdl.Event.mouse_button_down ->
      handle_mouse_button_down_event state
  | t when t = Sdl.Event.mouse_button_up ->
      handle_mouse_button_up_event state
  | t when t = Sdl.Event.mouse_motion ->
      handle_mouse_motion_event draw_at settings state
  | _ when mouse_wheel_direction state.event = Some `Up && not (ctrl_held ()) ->
      handle_mouse_wheel_event draw_at settings state `Up
  | _ when mouse_wheel_direction state.event = Some `Down && not (ctrl_held ())
    ->
      handle_mouse_wheel_event draw_at settings state `Down
  | _ when mouse_wheel_direction state.event = Some `Up && ctrl_held () ->
      settings.rotation <-
        ( settings.rotation
        +.
        if shift_held () then
          1.
        else
          5. ) ;
      state.draw_idx <- draw_at ~present_after_clear:false state.draw_idx
  | _ when mouse_wheel_direction state.event = Some `Down && ctrl_held () ->
      settings.rotation <-
        ( settings.rotation
        -.
        if shift_held () then
          1.
        else
          5. ) ;
      state.draw_idx <- draw_at ~present_after_clear:false state.draw_idx
  | _ ->
      ()
