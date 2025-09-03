# scope

An image file viewer written in OCaml with SDL

## Usage

```bash
scope image_path...
```

## Features

### Supported Formats

- PBM
- PGM
- PPM
- BMP

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
