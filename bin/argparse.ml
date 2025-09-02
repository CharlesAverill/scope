type arguments = {image_paths: string list}

let parse_arguments () =
  let image_paths = ref [] in
  let usage_msg = "Usage: scope image_path..." in
  Arg.parse [] (fun n -> image_paths := n :: !image_paths) usage_msg ;
  {image_paths= List.rev !image_paths}
