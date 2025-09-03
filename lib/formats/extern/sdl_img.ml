open Tsdl
open Tsdl_image
open Format

class sdl_img (filename : string) : format =
  object (self)
    val filename : string = filename

    method filename = filename

    val mutable _width : int = 0

    method width = _width

    val mutable _height : int = 0

    method height = _height

    val mutable surf : Sdl.surface Sdl.result = Error (`Msg "uninitialized")

    val _filetypes : string list = [".png"; ".jpg"; ".jpeg"]

    method filetypes = _filetypes

    initializer
      surf <- Image.load filename ;
      match surf with
      | Error _ ->
          ()
      | Ok surf ->
          let w, h = Sdl.get_surface_size surf in
          _width <- w ;
          _height <- h

    method valid : (unit, string) result =
      match surf with Ok _ -> Ok () | Error (`Msg s) -> Error s

    method save (_fn : string) : unit = () (* implement as needed *)

    method of_surf : (Sdl.surface -> format) option = None

    method to_surf : Sdl.surface option =
      match surf with Ok s -> Some s | Error _ -> None
  end
