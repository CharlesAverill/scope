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

| Control | Effect |
| --- | --- |
| Escape | Exits the application |
| Arrow Keys | Go to next or previous image in series |
| +/- | Zoom in/out |
| Mouse click + drag | Offset image position |

## Installation

```bash
git clone https://github.com/CharlesAverill/scope.git && cd scope
opam install . --deps-only
dune build
dune install
```
