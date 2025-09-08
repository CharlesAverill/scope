open Format

let formats : (module format_module) list =
  [ (module Netpbm.Pbm.PBM_Format)
  ; (module Netpbm.Pgm.PGM_Format)
  ; (module Netpbm.Ppm.PPM_Format)
  ; (module Bmp.BMP_Format)
  ; (module Extern.Sdl_img.SDL_Format) ]

let supported_filetypes =
  List.map (fun (module F : format_module) -> F.filetypes) formats
  |> List.flatten
  |> List.sort_uniq String.compare

let format_image (fn : string) : (format, string) result =
  match
    List.fold_left
      (fun a (module F : format_module) ->
        try F.f fn :: a with WrongExtension -> a )
      [] formats
  with
  | [] ->
      Error "No matching format"
  | h :: _ ->
      Ok h
