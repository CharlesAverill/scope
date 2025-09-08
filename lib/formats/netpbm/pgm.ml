(** PGM - https://netpbm.sourceforge.net/doc/pgm.html *)

open Tsdl
open Format
open Netpbm_utils

module PGM_Format : format_module = struct
  let filetypes = [".pgm"]

  (** Parse PGM header and return (cursor pos, magic number, width, height, maxval) *)
  let parse_pgm_header (filename : string) :
      (int64 * string * int * int * int) option =
    In_channel.with_open_text filename (fun ic ->
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

  class pgm (filename : string) : format =
    object (self)
      val filename : string = filename

      method filename = filename

      val mutable _width : int = 0

      method width = _width

      val mutable _height : int = 0

      method height = _height

      val mutable maxval : int = 0

      val mutable pixels : int array array = Array.make_matrix 0 0 0

      initializer
        if not (Utils.fn_matches filename filetypes) then
          raise WrongExtension
        else
          let pos, magic, width, height, mv =
            match parse_pgm_header filename with
            | Some x ->
                x
            | None ->
                raise (WrongFormat "couldn't parse header")
          in
          _width <- width ;
          _height <- height ;
          maxval <- mv ;
          match magic with
          | "P2" ->
              let pixel_data = get_text_pixels filename pos in
              pixels <-
                Array.init_matrix _height _width (fun y x ->
                    let idx = (y * _width) + x in
                    pixel_data.(idx) )
          | "P5" ->
              let data = get_bin_pixels filename pos in
              pixels <-
                Array.init_matrix _height _width (fun y x ->
                    let idx = (y * _width) + x in
                    try get_sample maxval data idx
                    with Invalid_argument _ -> 0 )
          | _ ->
              raise (WrongFormat "incorrect magic bytes")

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

      method to_surf : Sdl.surface option =
        if
          _width = 0 || _height = 0
          || match self#valid with Ok _ -> false | _ -> true
        then
          None
        else
          let open Bigarray in
          let ba =
            Array1.create Bigarray.Int32 Bigarray.C_layout (_width * _height)
          in
          for y = 0 to _height - 1 do
            for x = 0 to _width - 1 do
              let gray = pixels.(y).(x) in
              let g =
                if maxval = 255 then
                  gray
                else
                  gray * 255 / maxval
              in
              let g32 = Int32.of_int g in
              (* RGBA8888: R=G=B=g, A=255 *)
              let rgba =
                Int32.logor 0xFF000000l
                  (Int32.logor (Int32.shift_left g32 16)
                     (Int32.logor (Int32.shift_left g32 8) g32) )
              in
              ba.{(y * _width) + x} <- rgba
            done
          done ;
          Sdl.create_rgb_surface_with_format_from ba ~w:_width ~h:_height
            ~depth:32 ~pitch:_width Sdl.Pixel.format_abgr8888
          |> Utils.get_sdl_result
          |> fun x -> Some x
    end

  let f = new pgm
end
