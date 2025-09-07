# scope

An image file viewer written in OCaml with SDL

## Usage

```bash
scope image_path...
```

## Features

### Supported Formats

Custom parsers:
- [PBM](lib/formats/netpbm/pbm.ml)
- [PGM](lib/formats/netpbm/pgm.ml)
- [PPM](lib/formats/netpbm/ppm.ml)
- [BMP](lib/formats/bmp.ml)

Using external parsers:
- [PNG](lib/formats/extern/sdl_img.ml)
- [JPEG](lib/formats/extern/sdl_img.ml)

### Controls

| Function               | Control                     | Description                            |
| -------------------    | --------------------------- | -------------------------------------- |
| **Application**        | Escape                      | Exit the application                   |
|                        | F11                         | Toggle fullscreen                      |
| **File Operations**    | Ctrl + `o`                  | Open files                             |
| **Navigation**         | Arrow Keys                  | Go to next or previous image in series |
| **Image Manipulation** | `+` / `-` / Mouse Scroll    | Zoom in/out                            |
|                        | Mouse click + drag          | Offset image position                  |
|                        | Ctrl + Mouse Scroll         | Rotate                                 |
|                        | Shift + Ctrl + Mouse Scroll | Rotate slowly                          |
|                        | `f`                         | Fit image to window                    |
|                        | `h`                         | Flip horizontally                      |
|                        | `v`                         | Flip vertically                        |
|                        | `r`                         | Reset view                             |

## Installation

```bash
opam install scope
```

Or, from source:

```bash
git clone https://github.com/CharlesAverill/scope.git && cd scope
opam install . --deps-only
dune build
dune install
```
