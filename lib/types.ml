open Tsdl
open Formats.Format

let get_atomic (o : 'a) (mutex : Mutex.t) (f : 'a -> 'b) : 'b =
  Mutex.lock mutex ;
  let out = f o in
  Mutex.unlock mutex ; out

let set_atomic (o : 'a) (mutex : Mutex.t) (f : 'a -> unit) : unit =
  Mutex.lock mutex ; f o ; Mutex.unlock mutex

type t_settings =
  { mutable scale: float
  ; mutable offset: int * int
  ; mutable rotation: float
  ; mutable flip: Sdl.flip
  ; mutable render_stats: bool
  ; mutable new_render: bool
  ; mutable present_after_clear: bool
  ; mutable fit: bool
  ; mutex: Mutex.t }

let settings =
  { scale= 0.
  ; offset= (0, 0)
  ; rotation= 0.
  ; flip= Sdl.Flip.none
  ; render_stats= false
  ; new_render= false
  ; present_after_clear= false
  ; fit= false
  ; mutex= Mutex.create () }

let get_settings (f : t_settings -> 'a) : 'a =
  get_atomic settings settings.mutex f

let update_settings : (t_settings -> unit) -> unit =
  set_atomic settings settings.mutex

let default_settings () =
  update_settings (fun settings_obj ->
      settings_obj.scale <- 1. ;
      settings_obj.offset <- (0, 0) ;
      settings_obj.rotation <- 0. ;
      settings_obj.flip <- Sdl.Flip.none ;
      settings_obj.new_render <- false ;
      settings_obj.present_after_clear <- true ;
      settings_obj.fit <- false )

module ColorMap = Map.Make (Int)

type histogram = int ColorMap.t

type stats =
  {mutable misc: Sdl.texture option; mutable histogram: Sdl.texture option}

type image =
  { path: string
  ; mutable format: (format, string) result option
  ; mutable texture: Sdl.texture option
  ; stats: stats }

let img_array (image_paths : string list) : image array =
  Array.init (List.length image_paths) (fun i ->
      { path= List.nth image_paths i
      ; format= None
      ; texture= None
      ; stats= {misc= None; histogram= None} } )

type t_state =
  { mutable draw_idx: int
  ; mutable break: bool
  ; event: Sdl.event
  ; mutable dragging: bool
  ; mutable last_mouse_pos: int * int
  ; mutable imgs: image array
  ; mutex: Mutex.t }

let state =
  { draw_idx= 0
  ; break= false
  ; event= Sdl.Event.create ()
  ; dragging= false
  ; last_mouse_pos= (0, 0)
  ; imgs=
      Array.make 0
        { path= ""
        ; format= None
        ; texture= None
        ; stats= {misc= None; histogram= None} }
  ; mutex= Mutex.create () }

let get_state (f : t_state -> 'a) = get_atomic state state.mutex f

let update_state = set_atomic state state.mutex
