open Tsdl
open Types

let little_endian =
  let x = 1 in
  let ba = Bytes.create 4 in
  Bytes.set_int32_le ba 0 (Int32.of_int x) ;
  Bytes.get_int8 ba 0 = 1

let get_pixel pixels offset bpp =
  match bpp with
  | 1 ->
      pixels.{offset}
  | 2 ->
      if little_endian then
        pixels.{offset} lor (pixels.{offset + 1} lsl 8)
      else
        (pixels.{offset} lsl 8) lor pixels.{offset + 1}
  | 3 ->
      if little_endian then
        pixels.{offset}
        lor (pixels.{offset + 1} lsl 8)
        lor (pixels.{offset + 2} lsl 16)
      else
        (pixels.{offset} lsl 16)
        lor (pixels.{offset + 1} lsl 8)
        lor pixels.{offset + 2}
  | 4 ->
      if little_endian then
        pixels.{offset}
        lor (pixels.{offset + 1} lsl 8)
        lor (pixels.{offset + 2} lsl 16)
        lor (pixels.{offset + 3} lsl 24)
      else
        (pixels.{offset} lsl 24)
        lor (pixels.{offset + 1} lsl 16)
        lor (pixels.{offset + 2} lsl 8)
        lor pixels.{offset + 3}
  | _ ->
      0

let histogram_of_surface_hex (surface : Sdl.surface) : histogram =
  let hist = ref ColorMap.empty in
  Sdl.lock_surface surface |> Utils.get_sdl_result ;
  let pixels = Sdl.get_surface_pixels surface Bigarray.Int8_unsigned in
  Sdl.unlock_surface surface ;
  let w, h = Sdl.get_surface_size surface in
  let pitch = Sdl.get_surface_pitch surface in
  let fmt =
    Sdl.alloc_format (Sdl.get_surface_format_enum surface)
    |> Utils.get_sdl_result
  in
  let bpp = Sdl.get_pixel_format_bytes_pp fmt in
  for y = 0 to h - 1 do
    for x = 0 to w - 1 do
      let offset = (y * pitch) + (x * bpp) in
      let pixel = get_pixel pixels offset bpp in
      let r, g, b, _a = Sdl.get_rgba fmt (Int32.of_int pixel) in
      (* Encode as 24-bit hex color *)
      let hex = (r lsl 16) lor (g lsl 8) lor b in
      hist :=
        ColorMap.update hex
          (function None -> Some 1 | Some c -> Some (c + 1))
          !hist
    done
  done ;
  Sdl.free_format fmt ;
  !hist

(** [surface_of_histogram ~bar_width ~max_height hist] generates an SDL surface visualizing
    the histogram as vertical color bars. *)
let texture_of_histogram (renderer : Sdl.renderer) (hist : histogram) :
    Sdl.texture =
  let width = !Init.owin_w in
  let height = !Init.owin_h in
  (* Take at most 100 most frequent colors *)
  let top_colors =
    ColorMap.bindings hist
    |> List.sort (fun (_, c1) (_, c2) -> compare c2 c1) (* descending count *)
    |> fun l ->
    if List.length l > 100 then
      List.filteri (fun i _ -> i < 100) l
    else
      l
  in
  (* let num_colors = List.length top_colors in *)
  let bar_width = max 1 (width / 100) in
  (* Create a 32-bit RGBA texture with streaming access *)
  let tex =
    Sdl.create_texture renderer Sdl.Pixel.format_rgba8888
      Sdl.Texture.access_target ~w:width ~h:height
    |> Utils.get_sdl_result
  in
  (* Set the texture as render target *)
  Sdl.set_render_target renderer (Some tex) |> Utils.get_sdl_result ;
  (* Clear texture to fully transparent *)
  Sdl.set_render_draw_color renderer 0 0 0 0 |> Utils.get_sdl_result ;
  Sdl.render_clear renderer |> Utils.get_sdl_result ;
  (* Determine maximum count for scaling *)
  let max_count = List.fold_left (fun acc (_, c) -> max acc c) 0 top_colors in
  let log_max = log (1. +. float max_count) in
  (* Draw bars *)
  let x = ref 0 in
  List.iter
    (fun (color, count) ->
      let r = (color lsr 16) land 0xFF in
      let g = (color lsr 8) land 0xFF in
      let b = color land 0xFF in
      let bar_h =
        ( if max_count = 0 then
            0
          else
            int_of_float (log (1. +. float count) /. log_max *. float height) )
        / 4
      in
      let y = height - bar_h in
      (* Draw filled bar *)
      Sdl.set_render_draw_color renderer r g b 255 |> Utils.get_sdl_result ;
      let rect = Sdl.Rect.create ~x:!x ~y ~w:bar_width ~h:bar_h in
      Sdl.render_fill_rect renderer (Some rect) |> Utils.get_sdl_result ;
      (* Draw black outline *)
      Sdl.set_render_draw_color renderer 0 0 0 255 |> Utils.get_sdl_result ;
      Sdl.render_draw_rect renderer (Some rect) |> Utils.get_sdl_result ;
      x := !x + bar_width )
    top_colors ;
  (* Reset render target to default (window) *)
  Sdl.set_render_target renderer None |> Utils.get_sdl_result ;
  tex
