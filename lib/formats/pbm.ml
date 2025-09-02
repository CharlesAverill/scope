(** PBM - https://netpbm.sourceforge.net/doc/pbm.html *)

open Graphics
open Format

class pbm (filename : string) : format =
  object (self)
    val filename : string = filename

    method filename = filename

    val mutable _width : int = 0

    method width = _width

    val mutable _height : int = 0

    method height = _height

    val mutable pixels : color array array = Array.make_matrix 0 0 black

    val _filetypes : string list = [".pbm"]

    method filetypes = _filetypes

    initializer
      if not (List.exists (Filename.check_suffix filename) _filetypes) then
        raise WrongExtension
      else
        let tokens, pos = Utils.parse_pbm_header filename in
        let magic, height, width =
          match tokens with [m; h; w] -> (m, h, w) | _ -> raise WrongExtension
        in
        _width <- int_of_string width ;
        _height <- int_of_string height ;
        match magic with
        | "P1" ->
            let spixels =
              In_channel.with_open_text filename (fun ic ->
                  In_channel.seek ic (Int64.of_int pos) ;
                  In_channel.input_all ic )
              |> Utils.split_on_whitespace
            in
            let pixel_data = Array.of_list (List.map int_of_string spixels) in
            pixels <-
              Array.init_matrix _height _width (fun y x ->
                  let idx = (y * _width) + x in
                  if pixel_data.(idx) = 0 then
                    black
                  else
                    white )
        | "P4" ->
            let data =
              In_channel.with_open_bin filename (fun ic ->
                  In_channel.seek ic (Int64.of_int pos) ;
                  In_channel.input_all ic |> Bytes.of_string )
            in
            pixels <-
              Array.init_matrix _height _width (fun y x ->
                  let byte_idx = ((y * _width) + x) / 8 in
                  let bit_idx = 7 - (((y * _width) + x) mod 8) in
                  if
                    byte_idx < Bytes.length data
                    && (Bytes.get data byte_idx |> int_of_char)
                       land (1 lsl bit_idx)
                       <> 0
                  then
                    white
                  else
                    black )
        | _ ->
            failwith "Unsupported PBM format"

    method valid : (unit, string) result =
      if not (Array.length pixels = _height && Array.length pixels.(0) = _width)
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
