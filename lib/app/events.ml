open Tsdl

let is_plus event =
  let key = Sdl.Event.(get event keyboard_keycode) in
  let mods = Sdl.get_mod_state () in
  (key = Sdl.K.equals && mods land Sdl.Kmod.shift <> 0) || key = Sdl.K.kp_plus

let mouse_wheel_direction event =
  if Sdl.Event.(get event typ) = Sdl.Event.mouse_wheel then
    let y = Sdl.Event.(get event mouse_wheel_y) in
    if y > 0 then
      Some `Up
    else if y < 0 then
      Some `Down
    else
      None
  else
    None
