open Format
open Ppm
open Pbm

let formats : (string -> format) list = [new ppm; new pbm]

let format_images (fns : string list) : (format, string) result list =
  List.map
    (fun fn ->
      match
        List.fold_left
          (fun a c -> try c fn :: a with WrongExtension -> a)
          [] formats
      with
      | [] ->
          Error "No matching format"
      | h :: _ ->
          Ok h )
    fns
