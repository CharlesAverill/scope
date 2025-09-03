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
    | Some c ->
        In_channel.seek ic (Int64.pred (In_channel.pos ic))
        (* step back one char *)
    | None ->
        ()
  in
  loop ()
