(** BMP - supports 1,4,8,24-bit *)

open Tsdl
open Format
open Bigarray

let to_sdl_color_bmp r g b = Sdl.Color.create ~r ~g ~b ~a:255

let le16 buf pos =
  int_of_char (Bytes.get buf pos)
  lor (int_of_char (Bytes.get buf (pos + 1)) lsl 8)

let le32 buf pos =
  int_of_char (Bytes.get buf pos)
  lor (int_of_char (Bytes.get buf (pos + 1)) lsl 8)
  lor (int_of_char (Bytes.get buf (pos + 2)) lsl 16)
  lor (int_of_char (Bytes.get buf (pos + 3)) lsl 24)

class bmp (filename : string) : format =
  object (self)
    val filename : string = filename

    method filename = filename

    val mutable _width : int = 0

    method width = _width

    val mutable _height : int = 0

    method height = _height

    val mutable pixels : Sdl.color array array = [||]

    val mutable maxval : int = 255

    val _filetypes : string list = [".bmp"]

    method filetypes = _filetypes

    initializer
      if not (Utils.fn_matches filename _filetypes) then
        raise WrongExtension
      else
        let data =
          In_channel.with_open_bin filename (fun ic ->
              In_channel.input_all ic |> Bytes.of_string )
        in
        (* BMP header *)
        if Bytes.sub data 0 2 <> Bytes.of_string "BM" then
          raise (WrongFormat "incorrect magic bytes") ;
        let bfOffBits = le32 data 10 in
        _width <- le32 data 18 ;
        _height <- le32 data 22 ;
        let bpp = le16 data 28 in
        let colors_used = le32 data 46 in
        let row_size = ((_width * bpp) + 31) / 32 * 4 in
        let palette =
          match bpp with
          | 1 | 4 | 8 ->
              let n_colors =
                if colors_used > 0 then
                  colors_used
                else
                  1 lsl bpp
              in
              Array.init n_colors (fun i ->
                  let pos = 54 + (i * 4) in
                  let b = int_of_char (Bytes.get data pos) in
                  let g = int_of_char (Bytes.get data (pos + 1)) in
                  let r = int_of_char (Bytes.get data (pos + 2)) in
                  to_sdl_color_bmp r g b )
          | _ ->
              [||]
        in
        pixels <-
          Array.init _height (fun y ->
              Array.init _width (fun x ->
                  let row = _height - 1 - y in
                  let pos = bfOffBits + (row * row_size) in
                  match bpp with
                  | 1 ->
                      let byte = int_of_char (Bytes.get data (pos + (x / 8))) in
                      let bit = 7 - (x mod 8) in
                      palette.((byte lsr bit) land 1)
                  | 4 ->
                      let byte = int_of_char (Bytes.get data (pos + (x / 2))) in
                      let nibble =
                        if x mod 2 = 0 then
                          byte lsr 4
                        else
                          byte land 0xF
                      in
                      palette.(nibble)
                  | 8 ->
                      let idx = int_of_char (Bytes.get data (pos + x)) in
                      palette.(idx)
                  | 24 ->
                      let p = pos + (x * 3) in
                      let b = int_of_char (Bytes.get data p) in
                      let g = int_of_char (Bytes.get data (p + 1)) in
                      let r = int_of_char (Bytes.get data (p + 2)) in
                      to_sdl_color_bmp r g b
                  | _ ->
                      raise (WrongFormat "unsupported bitcount") ) )

    method valid : (unit, string) result =
      if Array.length pixels <> _height || Array.length pixels.(0) <> _width
      then
        Error
          (Printf.sprintf "pixel matrix shape is (%d, %d), expected (%d, %d)"
             (Array.length pixels)
             (Array.length pixels.(0))
             _width _height )
      else
        Ok ()

    method save (_fn : string) : unit = () (* implement as needed *)

    method of_surf : (Sdl.surface -> format) option = None

    method to_surf : Sdl.surface option =
      if
        _width = 0 || _height = 0
        || match self#valid with Ok _ -> false | _ -> true
      then
        None
      else
        let ba = Array1.create Int32 C_layout (_width * _height) in
        for y = 0 to _height - 1 do
          for x = 0 to _width - 1 do
            let c = pixels.(y).(x) in
            let r = Int32.of_int (Sdl.Color.r c) in
            let g = Int32.of_int (Sdl.Color.g c) in
            let b = Int32.of_int (Sdl.Color.b c) in
            let a = Int32.of_int (Sdl.Color.a c) in
            let rgba =
              Int32.logor (Int32.shift_left a 24)
                (Int32.logor (Int32.shift_left b 16)
                   (Int32.logor (Int32.shift_left g 8) r) )
            in
            ba.{(y * _width) + x} <- rgba
          done
        done ;
        Sdl.create_rgb_surface_with_format_from ba ~w:_width ~h:_height
          ~depth:32 ~pitch:_width Sdl.Pixel.format_abgr8888
        |> Result.get_ok
        |> fun x -> Some x
  end
