(lang dune 3.9)

(name scope)

(generate_opam_files true)

(source
 (github CharlesAverill/scope))

(authors "Charles Averill")

(maintainers "Charles Averill")

(license MIT)

(documentation https://github.com/CharlesAverill/scope)

(package
 (name scope)
 (synopsis "OCaml Image File Viewer")
 (description "An image file viewer written in OCaml with lablgtk")
 (depends
   (ocaml (>= 5.3.0))
   tsdl
 )
)
