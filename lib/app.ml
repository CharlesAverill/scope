open Graphics
open Manager

let init_app () : unit =
  open_graph "" ;
  set_window_title "Scope Image Viewer" ;
  auto_synchronize false

let main_loop (image_paths : string list) : unit =
  if image_paths = [] then
    ()
  else
    let formatted_imgs = format_images image_paths in
    let drawn = ref false in
    while true do
      let idx = 0 in
      match List.nth formatted_imgs idx with
      | Ok img when not !drawn ->
          resize_window img#width img#height ;
          set_window_title ("Scope Image Viewer - " ^ img#filename) ;
          Printf.printf "%d %d\n" img#width img#height ;
          flush stdout ;
          draw_image img#to_gr_img 0 0 ;
          synchronize () ;
          drawn := true
      | Ok _ ->
          ()
      | Error s ->
          Printf.printf "Error: %s\n" s ;
          exit 1
    done
