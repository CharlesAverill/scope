(lang dune 3.18)

(using dune_site 0.1)

(name scope)

(generate_opam_files true)

(source
 (github CharlesAverill/scope))

(authors "Charles Averill")

(maintainers "Charles Averill")
(maintenance_intent "(latest)")

(license MIT)

(documentation https://github.com/CharlesAverill/scope)

(package
 (name scope)
 (synopsis "OCaml Image File Viewer")
 (description "An image file viewer written in OCaml")
 (depends
   (ocaml (>= 5.3.0))
   tsdl
   tsdl-image
   dune-site
   (odoc :with-doc)
   (ocamlformat :with-doc))
  (sites (share app_data)))
