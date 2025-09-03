open Format

let formats : (string -> format) list =
  [ new Netpbm.Pbm.pbm
  ; new Netpbm.Pgm.pgm
  ; new Netpbm.Ppm.ppm
  ; new Bmp.bmp
  ; new Extern.Sdl_img.sdl_img ]

let format_image (fn : string) : (format, string) result =
  match
    List.fold_left
      (fun a c -> try c fn :: a with WrongExtension -> a)
      [] formats
  with
  | [] ->
      Error "No matching format"
  | h :: _ ->
      Ok h
