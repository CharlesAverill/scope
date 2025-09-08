open Tsdl
open Formats.Manager
open Logging
open Render
open Events
open Types

let quit () = Sdl.quit () ; exit 0

let remove_from_imgs (state : state) (idx : int) : unit =
  let n = Array.length state.imgs in
  if idx < 0 || idx >= n then
    invalid_arg "remove_from_array: index out of bounds" ;
  Tinyfiledialogs.(
    notify_popup ~title:"Scope Image Viewer"
      ~message:
        (Printf.sprintf "File %s could not be loaded" state.imgs.(idx).path)
      ~icon_type:Error ) ;
  state.imgs <-
    Array.init (n - 1) (fun i ->
        if i < idx then
          state.imgs.(i)
        else
          state.imgs.(i + 1) )

let rec draw_at window renderer settings state ?(present_after_clear = true)
    ?(new_render = false) ?(fit = false) i : int =
  let do_draw new_tex img i =
    if fit then fit_to_window img settings ;
    if new_render then
      Sdl.set_window_title window
        (state.imgs.(i).path ^ " - Scope Image File Viewer") ;
    try
      if new_tex then (
        let tex =
          Some (draw_texture window renderer img state.imgs.(i) None settings)
        in
        state.imgs.(i).format <- Some (Ok img) ;
        state.imgs.(i).texture <- tex
      ) else
        draw_texture window renderer img state.imgs.(i) state.imgs.(i).texture
          settings
        |> ignore ;
      i
    with InvalidImage ->
      remove_from_imgs state i ;
      draw_at ~present_after_clear ~new_render window renderer settings state i
  in
  if
    Array.for_all
      (fun i -> match i.format with Some (Error _) -> true | _ -> false)
      state.imgs
  then
    quit ()
  else (
    clear renderer ;
    if present_after_clear then Sdl.render_present renderer ;
    let i =
      if i < 0 || i >= Array.length state.imgs then
        abs (i - Array.length state.imgs) mod Array.length state.imgs
      else
        i
    in
    match state.imgs.(i).format with
    | Some (Ok img) ->
        do_draw false img i
    | Some (Error _) ->
        remove_from_imgs state i ;
        draw_at ~new_render ~present_after_clear window renderer settings state
          i
    | None -> (
      match format_image state.imgs.(i).path with
      | Ok img ->
          do_draw true img i
      | Error s ->
          _log Log_Error "Skipping invalid image '%s': %s\n ()"
            state.imgs.(i).path s ;
          remove_from_imgs state i ;
          draw_at ~present_after_clear ~new_render window renderer settings
            state i )
  )

let run window renderer (image_paths : string list) : unit =
  let settings =
    { scale= 1.
    ; offset= (0, 0)
    ; rotation= 0.
    ; flip= Sdl.Flip.none
    ; render_stats= false }
  in
  if image_paths = [] then exit 0 ;
  let state : state =
    { draw_idx= 0
    ; break= false
    ; event= Sdl.Event.create ()
    ; dragging= false
    ; last_mouse_pos= (0, 0)
    ; imgs= img_array image_paths }
  in
  let n () = Array.length state.imgs in
  let draw_at = draw_at window renderer settings state in
  (* First draw *)
  state.draw_idx <- draw_at ~new_render:true state.draw_idx ;
  (* Event loop *)
  while not state.break do
    Sdl.wait_event (Some state.event) |> Utils.get_sdl_result ;
    handle_event draw_at window n state settings Sdl.Event.(get state.event typ)
  done ;
  quit ()
