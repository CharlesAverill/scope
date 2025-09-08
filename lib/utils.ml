let fn_matches (filename : string) (filetypes : string list) : bool =
  List.exists (Filename.check_suffix filename) filetypes

let trim_nulls s =
  String.to_seq s
  |> Seq.filter (fun x -> 32 <= int_of_char x && int_of_char x <= 125)
  |> String.of_seq

let split_on_whitespace s = Str.split (Str.regexp "[ \t\n\r]+") (trim_nulls s)

let skip_whitespace (ic : In_channel.t) : unit =
  let rec loop () =
    match In_channel.input_char ic with
    | Some c when c = ' ' || c = '\t' || c = '\n' || c = '\r' ->
        loop ()
    | Some _ ->
        In_channel.seek ic (Int64.pred (In_channel.pos ic))
        (* step back one char *)
    | None ->
        ()
  in
  loop ()

let time_it ?(func_name : string = "func") f =
  let start = Unix.gettimeofday () in
  let out = f () in
  let stop = Unix.gettimeofday () in
  Printf.printf "Time of %s: %f\n" func_name (stop -. start) ;
  out

let remove_from_array (arr : 'a array ref) (idx : int) : unit =
  let n = Array.length !arr in
  if idx < 0 || idx >= n then
    invalid_arg "remove_from_array: index out of bounds" ;
  arr :=
    Array.init (n - 1) (fun i ->
        if i < idx then
          !arr.(i)
        else
          !arr.(i + 1) )

let same_file p1 p2 = Unix.realpath p1 = Unix.realpath p2

let get_sdl_result (r : ('a, 'b) result) : 'a =
  match r with
  | Ok x ->
      x
  | Error _ ->
      Logging.(fatal rc_SDL_Error "%s\n" (Tsdl.Sdl.get_error ()))
