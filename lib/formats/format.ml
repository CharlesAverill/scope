open Tsdl

exception WrongExtension

exception WrongFormat

(** Class type for a formatted image *)
class type format = object
  method valid : (unit, string) result
  (** Check whether the formatted image is valid *)

  method filetypes : string list
  (** The list of supported file extensions *)

  method filename : string
  (** Filename of the formatted image *)

  method height : int

  method width : int

  method save : string -> unit
  (** Save the image to a file *)

  method of_surf : (Sdl.surface -> format) option
  (** Convert from a [Sdl.surface], optional *)

  method to_surf : Sdl.surface option
  (** Convert to a [Sdl.surface] if valid *)
end
