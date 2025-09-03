open Tsdl
open Formats.Manager

let init_app () : Sdl.window * Sdl.renderer =
  Sdl.init Sdl.Init.video |> ignore ;
  let window =
    Sdl.create_window "Scope Image Viewer" ~x:Sdl.Window.pos_centered
      ~y:Sdl.Window.pos_centered ~w:800 ~h:600 Sdl.Window.shown
    |> Result.get_ok
  in
  let renderer =
    Sdl.create_renderer window ~index:(-1) ~flags:Sdl.Renderer.accelerated
    |> Result.get_ok
  in
  (window, renderer)

let draw_img window renderer img =
  Sdl.set_window_size window ~w:img#width ~h:img#height |> ignore ;
  Sdl.set_window_title window (img#filename ^ " - Scope Image File Viewer") ;
  Printf.printf "%s %d %d\n" img#filename img#width img#height ;
  flush stdout ;
  (* Convert img#to_gr_img to an SDL texture *)
  let surface = img#to_surf in
  let texture =
    Sdl.create_texture_from_surface renderer surface |> Result.get_ok
  in
  Sdl.render_clear renderer |> ignore ;
  Sdl.render_copy renderer texture |> ignore ;
  Sdl.render_present renderer ;
  Sdl.free_surface surface ;
  Sdl.destroy_texture texture

let main_loop window renderer (image_paths : string list) : unit =
  if image_paths = [] then
    ()
  else
    let n = List.length image_paths in
    let loaded_imgs = Array.make n None in
    let idx = ref 0 in
    let draw_at i =
      match loaded_imgs.(i) with
      | Some (Ok img) ->
          draw_img window renderer img
      | Some (Error s) ->
          Printf.printf "Error: %s\n" s ;
          exit 1
      | None -> (
        match format_image (List.nth image_paths i) with
        | Ok img ->
            loaded_imgs.(i) <- Some (Ok img) ;
            draw_img window renderer img
        | Error s ->
            Printf.printf "Error: %s\n" s ;
            exit 1 )
    in
    draw_at !idx ;
    let rec event_loop () =
      let event = Sdl.Event.create () in
      ( if Sdl.poll_event (Some event) then
          match Sdl.Event.(get event typ) with
          | t when t = Sdl.Event.quit ->
              Sdl.quit () ; exit 0
          | t when t = Sdl.Event.key_down -> (
            match Sdl.Event.(get event Sdl.Event.keyboard_keycode) with
            | k when k = Sdl.K.right ->
                if !idx < n - 1 then (
                  idx := !idx + 1 ;
                  draw_at !idx
                )
            | k when k = Sdl.K.left ->
                if !idx > 0 then (
                  idx := !idx - 1 ;
                  draw_at !idx
                )
            | k when k = Sdl.K.q || k = Sdl.K.escape ->
                Sdl.quit () ; exit 0
            | _ ->
                () )
          | _ ->
              () ) ;
      event_loop ()
    in
    event_loop ()
