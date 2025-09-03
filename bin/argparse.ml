type arguments = {image_paths: string list}

let collect_files path =
  if Sys.is_directory path then
    (* If directory, list all files inside *)
    Array.to_list (Sys.readdir path)
    |> List.map (Filename.concat path) (* prepend directory path *)
    |> List.sort String.compare |> List.rev
  else
    [path]

let parse_arguments () =
  let image_paths = ref [] in
  let usage_msg = "Usage: scope [image_path or folder]..." in
  Arg.parse []
    (fun n -> image_paths := collect_files n @ !image_paths)
    usage_msg ;
  {image_paths= List.rev !image_paths}
