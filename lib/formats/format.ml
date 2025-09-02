exception WrongExtension

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

  method of_gr_img : (Graphics.image -> format) option
  (** Convert from a Graphics.image, optional *)

  method to_gr_img : Graphics.image
  (** Convert to a Graphics.image *)
end
