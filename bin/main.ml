open Argparse
open Scope.App.Main_loop
open Scope.App.Init

let () =
  let args = Argparse.parse_arguments () in
  if 0 < List.length args.image_paths then
    let window, renderer = init_app () in
    run window renderer args.image_paths
