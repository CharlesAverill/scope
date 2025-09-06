open Tsdl
open Formats.Manager
open Logging
open Render
open Events
open Utils

let quit () = Sdl.quit () ; exit 0

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
  let state : state =
    { draw_idx= 0
    ; break= false
    ; event= Sdl.Event.create ()
    ; dragging= false
    ; last_mouse_pos= (0, 0) }
  in
  let draw_at = draw_at window renderer settings imgs in
  (* First draw *)
  state.draw_idx <-
    draw_at ~present_after_clear:true ~new_render:true state.draw_idx ;
  (* Event loop *)
  while not state.break do
    Sdl.wait_event (Some state.event) |> Result.get_ok ;
    handle_event draw_at n Sdl.Event.(get state.event typ) state settings
  done ;
  quit ()
