(** Get the next token in a PBM/PGM/PPM file *)
let next_token (ic : In_channel.t) : string option =
  let rec skip_and_read () =
    Utils.skip_whitespace ic ;
    match In_channel.input_char ic with
    | Some '#' ->
        (* Skip the rest of the line *)
        let rec skip_line () =
          match In_channel.input_char ic with
          | Some '\n' | None ->
              ()
          | Some _ ->
              skip_line ()
        in
        skip_line () ; skip_and_read ()
    | Some c when not (c = ' ' || c = '\t' || c = '\n' || c = '\r') ->
        let buf = Buffer.create 16 in
        Buffer.add_char buf c ;
        let rec read () =
          match In_channel.input_char ic with
          | Some d when not (d = ' ' || d = '\t' || d = '\n' || d = '\r') ->
              Buffer.add_char buf d ; read ()
          | Some _ | None ->
              Some (Buffer.contents buf)
        in
        read ()
    | Some _ ->
        skip_and_read ()
    | None ->
        None
  in
  skip_and_read ()

let get_text_pixels (filename : string) (pos : int64) : int array =
  Array.of_list
    (In_channel.with_open_text filename (fun ic ->
         In_channel.seek ic pos ;
         let rec aux tokens =
           match next_token ic with
           | None ->
               tokens
           | Some t ->
               aux (int_of_string t :: tokens)
         in
         List.rev (aux []) ) )

let get_bin_pixels (filename : string) (pos : int64) : bytes =
  In_channel.with_open_bin filename (fun ic ->
      In_channel.seek ic pos ;
      In_channel.input_all ic |> Bytes.of_string )

let get_sample maxval data i =
  if maxval < 256 then
    int_of_char (Bytes.get data i)
  else
    let hi = int_of_char (Bytes.get data i) in
    let lo = int_of_char (Bytes.get data (i + 1)) in
    (hi lsl 8) + lo
