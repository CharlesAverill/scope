(** PBM - https://netpbm.sourceforge.net/doc/pbm.html *)

open Tsdl
open Format
open Netpbm_utils

module PBM_Format : format_module = struct
  let filetypes = [".pbm"]

  (** Parse PBM header and return (cursor pos, magic number, width, height) *)
  let parse_pbm_header (filename : string) : (int64 * string * int * int) option
      =
    In_channel.with_open_text filename (fun ic ->
        match (next_token ic, next_token ic, next_token ic) with
        | Some magic, Some width, Some height ->
            Some
              ( In_channel.pos ic
              , magic
              , int_of_string width
              , int_of_string height )
        | _ ->
            None )

  class pbm (filename : string) : format =
    object (self)
      val filename : string = filename

      method filename = filename

      val mutable _width : int = 0

      method width = _width

      val mutable _height : int = 0

      method height = _height

      val mutable pixels : bool array array = Array.make_matrix 0 0 false

      initializer
        if not (Utils.fn_matches filename filetypes) then
          raise WrongExtension
        else
          let pos, magic, width, height =
            match parse_pbm_header filename with
            | Some x ->
                x
            | None ->
                raise WrongExtension
          in
          _width <- width ;
          _height <- height ;
          match magic with
          | "P1" ->
              let pixel_data = get_text_pixels filename pos in
              pixels <-
                Array.init_matrix _height _width (fun y x ->
                    let idx = (y * _width) + x in
                    pixel_data.(idx) = 1 )
          | "P4" ->
              let data = get_bin_pixels filename pos in
              pixels <-
                Array.init_matrix _height _width (fun y x ->
                    let byte_idx = ((y * _width) + x) / 8 in
                    let bit_idx = 7 - (((y * _width) + x) mod 8) in
                    byte_idx < Bytes.length data
                    && (Bytes.get data byte_idx |> int_of_char)
                       land (1 lsl bit_idx)
                       <> 0 )
          | _ ->
              raise (WrongFormat "incorrect magic bytes")

      method valid : (unit, string) result =
        if
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
            Array1.create Bigarray.Int8_unsigned C_layout (_width * _height)
          in
          for y = 0 to _height - 1 do
            for x = 0 to _width - 1 do
              ba.{(y * _width) + x} <-
                ( if pixels.(y).(x) then
                    1
                  else
                    0 )
            done
          done ;
          let surface =
            Sdl.create_rgb_surface_with_format_from ba ~w:_width ~h:_height
              ~depth:8 ~pitch:_width Sdl.Pixel.format_index8
            |> Utils.get_sdl_result
          in
          let palette =
            match Sdl.alloc_palette 2 with Ok p -> p | _ -> failwith ""
          in
          Utils.get_sdl_result
            (Sdl.set_palette_colors palette
               [ Sdl.Color.create ~r:0 ~g:0 ~b:0 ~a:255
               ; Sdl.Color.create ~r:255 ~g:255 ~b:255 ~a:255 ]
               ~fst:0 ) ;
          Utils.get_sdl_result (Sdl.set_surface_palette surface palette) ;
          Some surface
    end

  let f = new pbm
end
