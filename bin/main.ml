open Argparse
open Scope.App

let () =
  let args = Argparse.parse_arguments () in
  let window, renderer = init_app () in
  main_loop window renderer args.image_paths
