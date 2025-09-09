open Tsdl
open Formats.Manager
open Logging
open Render
open Events
open Types

let quit () = Sdl.quit () ; exit 0

let remove_from_imgs (idx : int) : unit =
  let imgs = get_state (fun state -> state.imgs) in
  let n = Array.length imgs in
  if idx < 0 || idx >= n then
    invalid_arg "remove_from_array: index out of bounds" ;
  ( if !Logging._GLOBAL_LOG_LEVEL = Log_Debug then
      Tinyfiledialogs.(
        notify_popup ~title:"Scope Image Viewer"
          ~message:
            (Printf.sprintf "File %s could not be loaded" imgs.(idx).path)
          ~icon_type:Error ) ) ;
  update_state (fun state ->
      state.imgs <-
        Array.init (n - 1) (fun i ->
            if i < idx then
              state.imgs.(i)
            else
              state.imgs.(i + 1) ) )

let rec draw_at window renderer ?(present_after_clear = true)
    ?(new_render = false) ?(fit = false) () : int =
  let imgs, i = get_state (fun state -> (state.imgs, state.draw_idx)) in
  let do_draw new_tex (img : Formats.Format.format) i =
    if fit then fit_to_window (Some (Ok img)) ;
    if new_render then
      Sdl.set_window_title window (imgs.(i).path ^ " - Scope Image File Viewer") ;
    try
      if new_tex then
        let tex = Some (draw_texture window renderer img imgs.(i) None) in
        update_state (fun state ->
            state.imgs.(i).format <- Some (Ok img) ;
            state.imgs.(i).texture <- tex )
      else
        draw_texture window renderer img imgs.(i) imgs.(i).texture |> ignore ;
      i
    with InvalidImage ->
      remove_from_imgs i ;
      draw_at ~present_after_clear ~new_render window renderer ()
  in
  if
    Array.for_all
      (fun i -> match i.format with Some (Error _) -> true | _ -> false)
      imgs
  then
    quit ()
  else (
    clear renderer ;
    if present_after_clear then Sdl.render_present renderer ;
    let i =
      if i < 0 || i >= Array.length imgs then
        abs (i - Array.length imgs) mod Array.length imgs
      else
        i
    in
    match imgs.(i).format with
    | Some (Ok img) ->
        do_draw false img i
    | Some (Error _) ->
        remove_from_imgs i ;
        draw_at ~new_render ~present_after_clear window renderer ()
    | None -> (
      match format_image imgs.(i).path with
      | Ok img ->
          do_draw true img i
      | Error s ->
          _log Log_Error "Skipping invalid image '%s': %s\n ()" imgs.(i).path s ;
          remove_from_imgs i ;
          draw_at ~present_after_clear ~new_render window renderer () )
  )

let event_thread window n () =
  while not (get_state (fun state -> state.break)) do
    Domain.cpu_relax () ;
    match pop_event_queue () with
    | Some ev ->
        handle_event window n ev
    | None ->
        ()
  done

let run window renderer (image_paths : string list) : unit =
  default_settings () ;
  if image_paths = [] then exit 0 ;
  update_state (fun state -> state.imgs <- img_array image_paths) ;
  let n () = Array.length (get_state (fun state -> state.imgs)) in
  let draw_at = draw_at window renderer in
  (* First draw *)
  let draw_idx = draw_at ~new_render:true () in
  update_state (fun state -> state.draw_idx <- draw_idx) ;
  let event_domain = Domain.spawn (event_thread window n) in
  (* Render loop *)
  let target_fps = 60 in
  let frame_delay =
    Int32.of_int (1000 / target_fps)
    (* in ms *)
  in
  while not (get_state (fun state -> state.break)) do
    let frame_start = Sdl.get_ticks () in
    (* Render *)
    let new_render, fit = get_settings (fun s -> (s.new_render, s.fit)) in
    (* Push events onto event queue *)
    let ev = ev () in
    ( if Sdl.poll_event (Some ev) then
        Sdl.Event.(
          push_event_queue
            { typ= get ev typ
            ; window_event_id= get ev window_event_id
            ; keyboard_keycode= get ev keyboard_keycode
            ; mouse_wheel_y= get ev mouse_wheel_y
            ; mouse_button_x= get ev mouse_button_x
            ; mouse_button_y= get ev mouse_button_y
            ; mouse_button_button= get ev mouse_button_button
            ; mouse_motion_x= get ev mouse_motion_x
            ; mouse_motion_y= get ev mouse_motion_y
            ; is_plus= is_plus ev
            ; ctrl_held= ctrl_held ()
            ; shift_held= shift_held () } ) ) ;
    let _ = draw_at ~present_after_clear:false ~new_render ~fit () in
    (* update_state (fun state -> state.draw_idx <- new_draw_idx) ; *)
    update_settings (fun settings ->
        settings.present_after_clear <- false ;
        settings.new_render <- false ;
        settings.fit <- false ) ;
    (* --- frame limiting --- *)
    let frame_time = Int32.(sub (Sdl.get_ticks ()) frame_start) in
    if Int32.(compare frame_time frame_delay < 0) then
      Sdl.delay Int32.(sub frame_delay frame_time)
  done ;
  Domain.join event_domain ;
  quit ()
