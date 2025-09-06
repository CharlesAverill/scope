open Scope.Logging

type arguments = {image_paths: string list}

let collect_files path =
  if Sys.is_directory path then
    (* If directory, list all files inside *)
    Array.to_list (Sys.readdir path)
    |> List.map (Filename.concat path) (* prepend directory path *)
    |> List.sort String.compare |> List.rev
  else
    [path]

let log_of_int = function
  | 0 ->
      Some Log_None
  | 1 ->
      Some Log_Debug
  | 2 ->
      Some Log_Info
  | 3 ->
      Some Log_Warning
  | 4 ->
      Some Log_Error
  | 5 ->
      Some Log_Critical
  | _ ->
      None

let log_of_string s =
  match String.trim (String.lowercase_ascii s) with
  | "none" ->
      Some Log_None
  | "debug" ->
      Some Log_Debug
  | "info" ->
      Some Log_Info
  | "warning" | "warn" ->
      Some Log_Warning
  | "error" ->
      Some Log_Error
  | "critical" | "fatal" ->
      Some Log_Critical
  | _ ->
      None

let parse_arguments () =
  let image_paths = ref [] in
  let usage_msg = "Usage: scope [options] [image_path or folder]..." in
  let speclist =
    [ ( "--log-level"
      , Arg.String
          (fun s ->
            match int_of_string_opt s with
            | Some n -> (
              match log_of_int n with
              | Some lvl ->
                  _GLOBAL_LOG_LEVEL := lvl
              | None ->
                  prerr_endline "Invalid log level number (0-5). Ignored." )
            | None -> (
              match log_of_string (String.lowercase_ascii s) with
              | Some lvl ->
                  _GLOBAL_LOG_LEVEL := lvl
              | None ->
                  prerr_endline
                    "Invalid log level string (none, debug, info, warning, \
                     error, critical). Ignored." ) )
      , "Set log level by number (0-5) or name (debug, info, ...)" )
    ; ( "--verbose"
      , Arg.Unit (fun () -> _GLOBAL_LOG_LEVEL := Log_Debug)
      , "Enable verbose logging" )
    ; ( "-v"
      , Arg.Unit (fun () -> _GLOBAL_LOG_LEVEL := Log_Debug)
      , "Enable verbose logging" ) ]
  in
  Arg.parse speclist
    (fun n -> image_paths := collect_files n @ !image_paths)
    usage_msg ;
  {image_paths= List.rev !image_paths}
