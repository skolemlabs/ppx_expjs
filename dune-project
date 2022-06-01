(lang dune 3.0)
(name ppx_expjs)

(source (github skolemlabs/ppx_expjs))

(generate_opam_files true)

(package
  (name ppx_expjs)
  (synopsis "ppx for exporting OCaml functions to be called from Javascript")
  (depends
    (dune (>= 3.0.0))
    (ppxlib (>= 0.26.0))
    (metaquot (>= 0.5.1))))
