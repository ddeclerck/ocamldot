(lang dune 2.1)
(name ocamldot)
(version 1.2.0)

(formatting disabled)
(generate_opam_files false)

(package
 (name ocamldot)
 (depends
  (ocaml (>= 4.03))
  (dune (>= 2.2))))

(package
 (name dot)
 (depends
  (ocaml (>= 4.03))
  (dune (>= 2.2))
   ocamldot))
