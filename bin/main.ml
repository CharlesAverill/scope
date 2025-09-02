open Argparse
open Scope.App

let () =
  let args = Argparse.parse_arguments () in
  init_app () ; main_loop args.image_paths
