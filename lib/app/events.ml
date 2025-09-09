open Tsdl
open Types
open Formats.Format

type event =
  { typ: int
  ; window_event_id: int
  ; keyboard_keycode: int
  ; mouse_wheel_y: int
  ; mouse_button_x: int
  ; mouse_button_y: int
  ; mouse_button_button: int
  ; mouse_motion_x: int
  ; mouse_motion_y: int
  ; is_plus: bool
  ; ctrl_held: bool
  ; shift_held: bool }

let event_queue : event Queue.t Atomic.t = Atomic.make (Queue.create ())

let pop_event_queue () = Queue.take_opt (Atomic.get event_queue)

let push_event_queue (ev : event) =
  Atomic.set event_queue
    (let q = Atomic.get event_queue in
     Queue.push ev q ; q )

let ev () : Sdl.event = get_state (fun state -> state.event)

let is_plus (event : Sdl.event) =
  let key = Sdl.Event.(get event keyboard_keycode) in
  let mods = Sdl.get_mod_state () in
  (key = Sdl.K.equals && mods land Sdl.Kmod.shift <> 0) || key = Sdl.K.kp_plus

let ctrl_held () : bool = Sdl.get_mod_state () land Sdl.Kmod.ctrl <> 0

let shift_held () : bool = Sdl.get_mod_state () land Sdl.Kmod.shift <> 0

let mouse_wheel_direction (ev : event) =
  if ev.typ = Sdl.Event.mouse_wheel then
    if ev.mouse_wheel_y > 0 then
      Some `Up
    else if ev.mouse_wheel_y < 0 then
      Some `Down
    else
      None
  else
    None

(* TODO : move this to main thread *)
let handle_window_event (window : Sdl.window) = function
  | we when we = Sdl.Event.window_event_resized ->
      (* Re-center and re-draw *)
      update_settings (fun settings -> settings.offset <- (0, 0)) ;
      let w, h = Sdl.get_window_size window in
      Init.owin_w := w ;
      Init.owin_h := h
  | _ ->
      ()

let flip_horizontal () : unit =
  Sdl.Flip.(
    update_settings (fun settings ->
        settings.flip <-
          ( if settings.flip = none then
              horizontal
            else if settings.flip = horizontal then
              none
            else if settings.flip = vertical then
              vertical + horizontal
            else
              vertical ) ) )

let flip_vertical () : unit =
  Sdl.Flip.(
    update_settings (fun settings ->
        settings.flip <-
          ( if settings.flip = none then
              vertical
            else if settings.flip = horizontal then
              horizontal + vertical
            else if settings.flip = vertical then
              none
            else
              horizontal ) ) )

(* TODO : move to main thread *)
let toggle_fullscreen (window : Sdl.window) =
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

let compute_fit_scale ?(upscale : bool = false) (fmt : format)
    ((win_w, win_h) : int * int) =
  (* Compute scaling factor to fit image within window *)
  let scale_w = float win_w /. float fmt#width in
  let scale_h = float win_h /. float fmt#height in
  (* don’t upscale, only shrink *)
  if upscale then
    min scale_w scale_h
  else
    min 1.0 (min scale_w scale_h)

let fit_to_window (fmt : (format, string) result option) =
  match fmt with
  | Some (Ok fmt) ->
      (* Compute scale to fit the window *)
      let win_w, win_h = (!Init.owin_w, !Init.owin_h) in
      let scale = compute_fit_scale ~upscale:true fmt (win_w, win_h) in
      update_settings (fun settings ->
          settings.offset <- (0, 0) ;
          settings.scale <- scale )
  | _ ->
      ()

let handle_key_down_event (window : Sdl.window) (n : unit -> int) (ev : event) :
    unit =
  (* Keycode *)
  match ev.keyboard_keycode with
  | k when k = Sdl.K.right || k = Sdl.K.down ->
      (* next image *)
      let draw_idx = get_state (fun state -> state.draw_idx) in
      let new_idx =
        if draw_idx < n () - 1 then
          draw_idx + 1
        else
          0
      in
      if new_idx <> draw_idx then (
        default_settings () ;
        update_settings (fun settings -> settings.new_render <- true) ;
        update_state (fun state -> state.draw_idx <- new_idx)
      )
  | k when k = Sdl.K.left || k = Sdl.K.up ->
      (* previous image *)
      let draw_idx = get_state (fun state -> state.draw_idx) in
      let new_idx =
        if draw_idx > 0 then
          draw_idx - 1
        else
          n () - 1
      in
      if new_idx <> draw_idx then (
        default_settings () ;
        update_settings (fun settings -> settings.new_render <- true) ;
        update_state (fun state -> state.draw_idx <- new_idx)
      )
  | k when k = Sdl.K.home ->
      (* go to first image *)
      let draw_idx = get_state (fun state -> state.draw_idx) in
      let new_idx = 0 in
      if new_idx <> draw_idx then (
        default_settings () ;
        update_settings (fun settings -> settings.new_render <- true) ;
        update_state (fun state -> state.draw_idx <- new_idx)
      )
  | k when k = Sdl.K.kend ->
      (* go to last image *)
      let draw_idx = get_state (fun state -> state.draw_idx) in
      let new_idx = n () - 1 in
      if new_idx <> draw_idx then (
        default_settings () ;
        update_settings (fun settings -> settings.new_render <- true) ;
        update_state (fun state -> state.draw_idx <- new_idx)
      )
  | k when k = Sdl.K.s ->
      (* render stats *)
      update_settings (fun settings ->
          settings.render_stats <- not settings.render_stats ) ;
      update_state (fun state -> state.imgs.(state.draw_idx).texture <- None)
  | k when k = Sdl.K.escape || (k = Sdl.K.w && ctrl_held ()) ->
      (* exit *)
      update_state (fun state -> state.break <- true)
  | _ when ev.is_plus ->
      (* zoom in *)
      update_settings (fun settings ->
          settings.scale <- min Init.max_zoom (settings.scale *. 1.25) ;
          settings.present_after_clear <- false )
  | k when k = Sdl.K.minus || k = Sdl.K.kp_minus ->
      (* zoom out *)
      update_settings (fun settings ->
          settings.scale <- max Init.min_zoom (settings.scale /. 1.25) ;
          settings.present_after_clear <- false )
  | k when k = Sdl.K.r && not ev.ctrl_held ->
      (* reset view *)
      default_settings ()
  | k when k = Sdl.K.r && ev.ctrl_held ->
      (* reload image view *)
      default_settings () ;
      update_state (fun state ->
          state.imgs.(state.draw_idx) <-
            { path= state.imgs.(state.draw_idx).path
            ; format= None
            ; texture= None
            ; stats= {misc= None; histogram= None} } ) ;
      update_settings (fun settings -> settings.new_render <- true)
  | k when k = Sdl.K.f ->
      (* fit to window *)
      fit_to_window (get_state (fun state -> state.imgs.(state.draw_idx).format))
  | k when k = Sdl.K.h ->
      (* flip horizontally *)
      flip_horizontal ()
  | k when k = Sdl.K.v ->
      (* flip vertically *)
      flip_vertical ()
  | k when k = Sdl.K.f11 ->
      (* toggle fullscreen *)
      toggle_fullscreen window ;
      update_settings (fun settings -> settings.present_after_clear <- true)
  | k when k = Sdl.K.o && ev.ctrl_held -> (
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
          let imgs = get_state (fun state -> state.imgs) in
          match
            List.filter
              (fun path ->
                not
                  (Array.exists
                     (fun img -> Utils.same_file img.path path)
                     imgs ) )
              (String.split_on_char '|' files)
          with
          | [] ->
              ()
          | files ->
              update_state (fun state ->
                  state.draw_idx <- n () ;
                  state.imgs <- Array.append state.imgs (img_array files) ) ;
              default_settings () ;
              update_settings (fun settings -> settings.new_render <- true) ) )
  | _ ->
      ()

let handle_mouse_button_down_event (ev : event) : unit =
  if ev.mouse_button_button = Sdl.Button.left then
    update_state (fun state ->
        state.dragging <- true ;
        state.last_mouse_pos <- (ev.mouse_button_x, ev.mouse_button_y) )

let handle_mouse_button_up_event (ev : event) : unit =
  if ev.mouse_button_button = Sdl.Button.left then
    update_state (fun state -> state.dragging <- false)

let handle_mouse_motion_event (ev : event) : unit =
  let dragging, last_mouse_pos =
    get_state (fun state -> (state.dragging, state.last_mouse_pos))
  in
  if dragging then (
    let x = ev.mouse_motion_x and y = ev.mouse_motion_y in
    let lx, ly = last_mouse_pos in
    let dx = x - lx and dy = y - ly in
    let ox, oy = get_settings (fun settings -> settings.offset) in
    update_settings (fun settings ->
        settings.offset <- (ox + dx, oy + dy) ;
        settings.present_after_clear <- false ) ;
    update_state (fun state -> state.last_mouse_pos <- (x, y))
  )

let handle_mouse_wheel_event direction =
  update_settings (fun settings ->
      settings.scale <-
        max Init.min_zoom
          (let f =
             match direction with `Up -> Float.mul | `Down -> Float.div
           in
           f settings.scale 1.25 ) ;
      settings.present_after_clear <- false )

let handle_event (window : Sdl.window) (n : unit -> int) (ev : event) =
  match ev.typ with
  | t when t = Sdl.Event.quit ->
      update_state (fun state -> state.break <- true)
  | t when t = Sdl.Event.window_event ->
      handle_window_event window ev.window_event_id
  | t when t = Sdl.Event.key_down ->
      handle_key_down_event window n ev
  | t when t = Sdl.Event.mouse_button_down ->
      handle_mouse_button_down_event ev
  | t when t = Sdl.Event.mouse_button_up ->
      handle_mouse_button_up_event ev
  | t when t = Sdl.Event.mouse_motion ->
      handle_mouse_motion_event ev
  | _ when mouse_wheel_direction ev = Some `Up && not (ctrl_held ()) ->
      handle_mouse_wheel_event `Up
  | _ when mouse_wheel_direction ev = Some `Down && not (ctrl_held ()) ->
      handle_mouse_wheel_event `Down
  | _ when mouse_wheel_direction ev = Some `Up && ctrl_held () ->
      update_settings (fun settings ->
          settings.rotation <-
            ( settings.rotation
            +.
            if ev.shift_held then
              1.
            else
              5. ) ;
          settings.present_after_clear <- false )
  | _ when mouse_wheel_direction ev = Some `Down && ctrl_held () ->
      update_settings (fun settings ->
          settings.rotation <-
            ( settings.rotation
            -.
            if ev.shift_held then
              1.
            else
              5. ) ;
          settings.present_after_clear <- false )
  | _ ->
      ()
