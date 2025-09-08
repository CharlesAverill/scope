open Tsdl
open Tsdl_ttf
open Types

exception Font_error of string

(** [render_text renderer font text color] renders a string as a texture. *)
let render_text renderer font text (r, g, b) =
  match
    Ttf.render_text_blended font text (Sdl.Color.create ~r ~g ~b ~a:255)
  with
  | Error (`Msg e) ->
      raise (Font_error e)
  | Ok surface ->
      let tex =
        Sdl.create_texture_from_surface renderer surface |> Utils.get_sdl_result
      in
      Sdl.free_surface surface ; tex

(** [texture_of_stats renderer font path surface hist] creates a texture showing
    stats for the given image. *)
let texture_of_stats renderer path (hist : histogram) =
  let font = Ttf.open_font Init.courier_ttf_path 48 |> Utils.get_sdl_result in
  let width, height = (!Init.owin_w, !Init.owin_h) in
  let num_colors = ColorMap.cardinal hist in
  (* Optional: compute average color *)
  let total_pixels = width * height in
  let sum_r, sum_g, sum_b =
    let hist_list = ColorMap.bindings hist in
    List.fold_left
      (fun (sr, sg, sb) (color, count) ->
        ( sr + ((color lsr 16) land 0xFF * count)
        , sg + ((color lsr 8) land 0xFF * count)
        , sb + (color land 0xFF * count) ) )
      (0, 0, 0) hist_list
  in
  let avg_r = sum_r / max 1 total_pixels in
  let avg_g = sum_g / max 1 total_pixels in
  let avg_b = sum_b / max 1 total_pixels in
  let lines =
    [ Printf.sprintf "%s" path
    ; Printf.sprintf "Size: %dx%d" width height
    ; Printf.sprintf "# Colors: %d" num_colors
    ; Printf.sprintf "Avg Color: R=%d G=%d B=%d" avg_r avg_g avg_b ]
  in
  (* Render each line as a texture *)
  let line_texs =
    List.map
      (fun line -> (line, render_text renderer font line (255, 255, 255)))
      lines
  in
  (* Get line heights *)
  let _, line_h = Ttf.size_text font "Hg" |> Utils.get_sdl_result in
  (* Create final texture *)
  let tex_h = line_h * List.length lines in
  let tex_w = !Init.owin_w in
  let tex =
    Sdl.create_texture renderer Sdl.Pixel.format_rgba8888
      Sdl.Texture.access_target ~w:tex_w ~h:tex_h
    |> Utils.get_sdl_result
  in
  (* Set render target and clear *)
  Sdl.set_render_target renderer (Some tex) |> Utils.get_sdl_result ;
  Sdl.set_render_draw_color renderer 0 0 0 255 |> Utils.get_sdl_result ;
  Sdl.render_clear renderer |> Utils.get_sdl_result ;
  (* Copy each line *)
  let y = ref 0 in
  List.iter
    (fun (line, tex) ->
      let w, h = Ttf.size_text font line |> Utils.get_sdl_result in
      let dst =
        Sdl.Rect.create ~x:0 ~y:!y
          ~w:
            ( if w > tex_w then
                tex_w
              else
                w )
          ~h
      in
      Sdl.render_copy renderer tex ~dst |> Utils.get_sdl_result ;
      Sdl.destroy_texture tex ;
      y := !y + h )
    line_texs ;
  (* Reset render target *)
  Sdl.set_render_target renderer None |> Utils.get_sdl_result ;
  Ttf.close_font font ;
  tex
