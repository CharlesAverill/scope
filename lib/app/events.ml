open Tsdl
open Render

type state =
  { mutable draw_idx: int
  ; mutable break: bool
  ; event: Sdl.event
  ; mutable dragging: bool
  ; mutable last_mouse_pos: int * int }

let is_plus event =
  let key = Sdl.Event.(get event keyboard_keycode) in
  let mods = Sdl.get_mod_state () in
  (key = Sdl.K.equals && mods land Sdl.Kmod.shift <> 0) || key = Sdl.K.kp_plus

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

let handle_event
    (draw_at : ?present_after_clear:bool -> ?new_render:bool -> int -> int) n ev
    (state : state) (settings : settings) =
  match ev with
  | t when t = Sdl.Event.quit ->
      state.break <- true
  | t when t = Sdl.Event.window_event -> (
    match Sdl.Event.(get state.event window_event_id) with
    | we when we = Sdl.Event.window_event_resized ->
        (* Reset offset so image is re-centered *)
        settings.offset <- (0, 0) ;
        (* Redraw background + image *)
        state.draw_idx <- draw_at state.draw_idx
    | _ ->
        () )
  | t when t = Sdl.Event.key_down -> (
    match Sdl.Event.(get state.event Sdl.Event.keyboard_keycode) with
    | k when k = Sdl.K.right || k = Sdl.K.down ->
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
    | k when k = Sdl.K.escape ->
        state.break <- true
    | _ when is_plus state.event ->
        settings.scale <- min max_zoom (settings.scale *. 1.25) ;
        state.draw_idx <- draw_at ~present_after_clear:false state.draw_idx
    | k when k = Sdl.K.minus || k = Sdl.K.kp_minus ->
        settings.scale <- max min_zoom (settings.scale /. 1.25) ;
        state.draw_idx <- draw_at ~present_after_clear:false state.draw_idx
    | _ ->
        () )
  | t when t = Sdl.Event.mouse_button_down ->
      let x = Sdl.Event.(get state.event mouse_button_x)
      and y = Sdl.Event.(get state.event mouse_button_y) in
      if Sdl.Event.(get state.event mouse_button_button) = Sdl.Button.left then (
        state.dragging <- true ;
        state.last_mouse_pos <- (x, y)
      )
  | t when t = Sdl.Event.mouse_button_up ->
      if Sdl.Event.(get state.event mouse_button_button) = Sdl.Button.left then
        state.dragging <- false
  | t when t = Sdl.Event.mouse_motion ->
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
  | _ when mouse_wheel_direction state.event = Some `Up ->
      settings.scale <- max min_zoom (settings.scale *. 1.25) ;
      draw_at ~present_after_clear:false state.draw_idx |> ignore
  | _ when mouse_wheel_direction state.event = Some `Down ->
      settings.scale <- max min_zoom (settings.scale /. 1.25) ;
      draw_at ~present_after_clear:false state.draw_idx |> ignore
  | _ ->
      ()
