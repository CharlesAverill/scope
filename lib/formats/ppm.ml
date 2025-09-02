(** PPM - https://netpbm.sourceforge.net/doc/ppm.html *)

open Graphics
open Format

class ppm (filename : string) : format =
  object (self)
    val filename : string = filename

    method filename = filename

    val mutable _width : int = 0

    method width = _width

    val mutable _height : int = 0

    method height = _height

    val mutable maxval : int = 0

    val mutable pixels : color array array = Array.make_matrix 0 0 black

    val _filetypes : string list = [".ppm"]

    method filetypes = _filetypes

    initializer
      if not (List.exists (Filename.check_suffix filename) _filetypes) then
        raise WrongExtension
      else
        let tokens, pos = Utils.parse_ppm_header filename in
        let magic, height, width, maxval_str =
          match tokens with
          | [m; h; w; mv] ->
              (m, h, w, mv)
          | _ ->
              raise WrongExtension
        in
        _width <- int_of_string width ;
        _height <- int_of_string height ;
        maxval <- int_of_string maxval_str ;
        (* --- Step 2: read pixel data depending on format --- *)
        match magic with
        | "P3" ->
            (* ASCII PPM *)
            let spixels =
              In_channel.with_open_text filename (fun ic ->
                  In_channel.seek ic (Int64.of_int pos) ;
                  In_channel.input_all ic )
              |> Utils.split_on_whitespace
            in
            let pixel_data = Array.of_list (List.map int_of_string spixels) in
            pixels <-
              Array.init_matrix _height _width (fun y x ->
                  let idx = ((y * _width) + x) * 3 in
                  let r = pixel_data.(idx) in
                  let g = pixel_data.(idx + 1) in
                  let b = pixel_data.(idx + 2) in
                  Graphics.rgb r g b )
        | "P6" ->
            (* Binary PPM *)
            let sample_size =
              if maxval < 256 then
                1
              else
                2
            in
            let data =
              In_channel.with_open_bin filename (fun ic ->
                  In_channel.seek ic (Int64.of_int pos) ;
                  In_channel.input_all ic |> Bytes.of_string )
            in
            let get_sample i =
              if sample_size = 1 then
                int_of_char (Bytes.get data i)
              else
                let hi = int_of_char (Bytes.get data i) in
                let lo = int_of_char (Bytes.get data (i + 1)) in
                (hi lsl 8) + lo
            in
            pixels <-
              Array.init_matrix _height _width (fun y x ->
                  let idx = ((y * _width) + x) * 3 * sample_size in
                  try
                    let r = get_sample idx in
                    let g = get_sample (idx + sample_size) in
                    let b = get_sample (idx + (2 * sample_size)) in
                    Graphics.rgb r g b
                  with Invalid_argument _ -> Graphics.magenta )
        | _ ->
            failwith "Unsupported PPM format"

    method valid : (unit, string) result =
      if not (maxval < 65536 && maxval > 0) then
        Error "maxval must be in (0, 65536)"
      else if
        not (Array.length pixels = _height && Array.length pixels.(0) = _width)
      then
        Error
          (Printf.sprintf "pixel matrix shape is (%d, %d), expected (%d, %d)"
             (Array.length pixels)
             (Array.length pixels.(0))
             _width _height )
      else
        Ok ()

    method save (_fn : string) : unit = () (* implement as needed *)

    method of_gr_img : (Graphics.image -> format) option = None

    method to_gr_img : Graphics.image =
      if _width = 0 || _height = 0 then
        create_image 0 0
      else
        Graphics.make_image pixels
  end
