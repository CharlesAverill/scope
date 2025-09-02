let split_on_whitespace s = Str.split (Str.regexp "[ \t\n\r]+") s

let parse_pbm_header (filename : string) : string list * int =
  In_channel.with_open_text filename (fun ic ->
      let rec aux (tokens : string list) (pos : int) =
        if List.length tokens >= 3 then
          (List.rev tokens, pos)
        else
          let line_start = In_channel.pos ic in
          match In_channel.input_line ic with
          | None ->
              (List.rev tokens, Int64.to_int line_start)
          | Some line ->
              let trimmed = String.trim line in
              if trimmed = "" || String.starts_with ~prefix:"#" trimmed then
                aux tokens (Int64.to_int (In_channel.pos ic))
              else
                let new_tokens = split_on_whitespace trimmed @ tokens in
                aux new_tokens (Int64.to_int (In_channel.pos ic))
      in
      aux [] 0 )

let parse_ppm_header (filename : string) : string list * int =
  In_channel.with_open_text filename (fun ic ->
      let rec aux (tokens : string list) (pos : int) =
        if List.length tokens >= 4 then
          (List.rev tokens, pos)
        else
          let line_start = In_channel.pos ic in
          match In_channel.input_line ic with
          | None ->
              (List.rev tokens, Int64.to_int line_start)
          | Some line ->
              let trimmed = String.trim line in
              if trimmed = "" || String.starts_with ~prefix:"#" trimmed then
                aux tokens (Int64.to_int (In_channel.pos ic))
              else
                let new_tokens = split_on_whitespace trimmed @ tokens in
                aux new_tokens (Int64.to_int (In_channel.pos ic))
      in
      aux [] 0 )
