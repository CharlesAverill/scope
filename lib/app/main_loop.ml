open Tsdl
open Formats.Manager
open Formats.Format
open Logging
open Render
open Events
open Utils

let default_settings settings =
  settings.scale <- 1. ;
  settings.offset <- (0, 0)

let quit () = Sdl.quit () ; exit 0

type image =
  { path: string
  ; mutable format: (format, string) result option
  ; mutable texture: Sdl.texture option }

let rec draw_at window renderer settings imgs ?(present_after_clear = true)
    ?(new_render = false) i : int =
  if
    Array.for_all
      (fun i -> match i.format with Some (Error _) -> true | _ -> false)
      !imgs
  then
    quit ()
  else (
    clear window renderer ;
    if present_after_clear then Sdl.render_present renderer ;
    let i =
      if i < 0 || i >= Array.length !imgs then
        abs (i - Array.length !imgs) mod Array.length !imgs
      else
        i
    in
    match !imgs.(i).format with
    | Some (Ok img) ->
        if new_render then
          Sdl.set_window_title window
            (!imgs.(i).path ^ " - Scope Image File Viewer") ;
        draw_texture window renderer img !imgs.(i).texture settings |> ignore ;
        i
    | Some (Error _) ->
        remove_from_array imgs i ;
        draw_at ~new_render ~present_after_clear window renderer settings imgs i
    | None -> (
      match format_image !imgs.(i).path with
      | Ok img -> (
          if new_render then
            Sdl.set_window_title window
              (!imgs.(i).path ^ " - Scope Image File Viewer") ;
          try
            let tex = Some (draw_texture window renderer img None settings) in
            !imgs.(i).format <- Some (Ok img) ;
            !imgs.(i).texture <- tex ;
            i
          with InvalidImage ->
            remove_from_array imgs i ;
            draw_at ~present_after_clear ~new_render window renderer settings
              imgs i )
      | Error s ->
          _log Log_Error "Skipping invalid image '%s': %s\n ()" !imgs.(i).path s ;
          remove_from_array imgs i ;
          draw_at ~present_after_clear ~new_render window renderer settings imgs
            i )
  )

let run window renderer (image_paths : string list) : unit =
  let settings = {scale= 1.; offset= (0, 0)} in
  if image_paths = [] then exit 0 ;
  let imgs =
    ref
      (Array.init (List.length image_paths) (fun i ->
           {path= List.nth image_paths i; format= None; texture= None} ) )
  in
  let n () = Array.length !imgs in
  let idx = ref 0 in
  let draw_at = draw_at window renderer settings imgs in
  (* First draw *)
  idx := draw_at ~present_after_clear:true ~new_render:true !idx ;
  (* Event loop *)
  let break = ref false in
  let event = Sdl.Event.create () in
  let dragging = ref false in
  let last_mouse_pos = ref (0, 0) in
  while not !break do
    Sdl.wait_event (Some event) |> Result.get_ok ;
    match Sdl.Event.(get event typ) with
    | t when t = Sdl.Event.quit ->
        break := true
    | t when t = Sdl.Event.window_event -> (
      match Sdl.Event.(get event window_event_id) with
      | we when we = Sdl.Event.window_event_resized ->
          (* Reset offset so image is re-centered *)
          settings.offset <- (0, 0) ;
          (* Redraw background + image *)
          ignore (draw_at !idx)
      | _ ->
          () )
    | t when t = Sdl.Event.key_down -> (
      match Sdl.Event.(get event Sdl.Event.keyboard_keycode) with
      | k when k = Sdl.K.right || k = Sdl.K.down ->
          let new_idx =
            if !idx < n () - 1 then
              !idx + 1
            else
              0
          in
          if new_idx <> !idx then (
            default_settings settings ;
            idx := draw_at ~new_render:true new_idx
          )
      | k when k = Sdl.K.left || k = Sdl.K.up ->
          let new_idx =
            if !idx > 0 then
              !idx - 1
            else
              n () - 1
          in
          if new_idx <> !idx then (
            default_settings settings ;
            idx := draw_at ~new_render:true new_idx
          )
      | k when k = Sdl.K.escape ->
          break := true
      | _ when is_plus event ->
          settings.scale <- min max_zoom (settings.scale *. 1.25) ;
          draw_at ~present_after_clear:false !idx |> ignore
      | k when k = Sdl.K.minus || k = Sdl.K.kp_minus ->
          settings.scale <- max min_zoom (settings.scale /. 1.25) ;
          draw_at ~present_after_clear:false !idx |> ignore
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
          draw_at ~present_after_clear:false !idx |> ignore
        )
    | _ when mouse_wheel_direction event = Some `Up ->
        settings.scale <- max min_zoom (settings.scale *. 1.25) ;
        draw_at ~present_after_clear:false !idx |> ignore
    | _ when mouse_wheel_direction event = Some `Down ->
        settings.scale <- max min_zoom (settings.scale /. 1.25) ;
        draw_at ~present_after_clear:false !idx |> ignore
    | _ ->
        ()
  done ;
  quit ()
