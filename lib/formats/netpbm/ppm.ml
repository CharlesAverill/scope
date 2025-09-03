(** PPM - https://netpbm.sourceforge.net/doc/ppm.html *)

open Tsdl
open Format
open Netpbm_utils

let to_sdl_color maxval r g b =
  Sdl.Color.create
    ~r:(r * 255 / maxval)
    ~g:(g * 255 / maxval)
    ~b:(b * 255 / maxval)
    ~a:255

(** Parse PPM header and return (cursor pos, magic number, width, height, maxval) *)
let parse_ppm_header (filename : string) :
    (int64 * string * int * int * int) option =
  In_channel.with_open_text filename (fun ic ->
      let line_start = In_channel.pos ic in
      match (next_token ic, next_token ic, next_token ic, next_token ic) with
      | Some magic, Some width, Some height, Some maxval ->
          Some
            ( In_channel.pos ic
            , magic
            , int_of_string width
            , int_of_string height
            , int_of_string maxval )
      | _ ->
          None )

class ppm (filename : string) : format =
  object (self)
    val filename : string = filename

    method filename = filename

    val mutable _width : int = 0

    method width = _width

    val mutable _height : int = 0

    method height = _height

    val mutable maxval : int = 0

    val mutable pixels : Sdl.color array array =
      Array.make_matrix 0 0 (to_sdl_color 255 255 0 255)

    val _filetypes : string list = [".ppm"]

    method filetypes = _filetypes

    initializer
      if not (Utils.fn_matches filename _filetypes) then
        raise WrongExtension
      else
        let pos, magic, width, height, maxval =
          match parse_ppm_header filename with
          | Some x ->
              x
          | None ->
              raise WrongFormat
        in
        _width <- width ;
        _height <- height ;
        match magic with
        | "P3" ->
            let pixel_data = get_text_pixels filename pos in
            pixels <-
              Array.init_matrix _height _width (fun y x ->
                  let idx = ((y * _width) + x) * 3 in
                  to_sdl_color maxval pixel_data.(idx)
                    pixel_data.(idx + 1)
                    pixel_data.(idx + 2) )
        | "P6" ->
            let data = get_bin_pixels filename pos in
            pixels <-
              Array.init_matrix _height _width (fun y x ->
                  let idx = ((y * _width) + x) * 3 in
                  try
                    to_sdl_color maxval
                      (get_sample maxval data idx)
                      (get_sample maxval data (idx + 1))
                      (get_sample maxval data (idx + 2))
                  with Invalid_argument _ -> to_sdl_color maxval 0 0 0 )
        | _ ->
            raise WrongFormat

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

    method of_surf : (Sdl.surface -> format) option = None

    method to_surf : Sdl.surface =
      if _width = 0 || _height = 0 then
        Sdl.create_rgb_surface ~w:1 ~h:1 ~depth:32 0l 0l 0l 0l |> Result.get_ok
      else
        let open Bigarray in
        let ba =
          Array1.create Bigarray.Int32 Bigarray.C_layout (_width * _height)
        in
        for y = 0 to _height - 1 do
          for x = 0 to _width - 1 do
            let c = pixels.(y).(x) in
            let r = Int32.of_int (Sdl.Color.r c) in
            let g = Int32.of_int (Sdl.Color.g c) in
            let b = Int32.of_int (Sdl.Color.b c) in
            let a = Int32.of_int (Sdl.Color.a c) in
            (* Pack as RGBA8888 *)
            let rgba =
              Int32.logor (Int32.shift_left r 24)
                (Int32.logor (Int32.shift_left g 16)
                   (Int32.logor (Int32.shift_left b 8) a) )
            in
            ba.{(y * _width) + x} <- rgba
          done
        done ;
        Sdl.create_rgb_surface_with_format_from ba ~w:_width ~h:_height
          ~depth:32 ~pitch:(_width * 4) Sdl.Pixel.format_abgr8888
        |> Result.get_ok
  end
