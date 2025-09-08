open Tsdl
open Formats.Format

type settings =
  { mutable scale: float
  ; mutable offset: int * int
  ; mutable rotation: float
  ; mutable flip: Sdl.flip
  ; mutable render_stats: bool }

let default_settings (settings : settings) =
  settings.scale <- 1. ;
  settings.offset <- (0, 0) ;
  settings.rotation <- 0. ;
  settings.flip <- Sdl.Flip.none

module ColorMap = Map.Make (Int)

type histogram = int ColorMap.t

type stats = {mutable histogram: histogram option}

type image =
  { path: string
  ; mutable format: (format, string) result option
  ; mutable texture: Sdl.texture option
  ; stats: stats }

type state =
  { mutable draw_idx: int
  ; mutable break: bool
  ; event: Sdl.event
  ; mutable dragging: bool
  ; mutable last_mouse_pos: int * int
  ; mutable imgs: image array }
