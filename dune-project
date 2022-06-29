(lang dune 3.0)
(name ppx_expjs)

(source (github skolemlabs/ppx_expjs))

(generate_opam_files true)

(authors "Zach Baylin <zach@skolem.tech>")
(maintainers "Zach Baylin <zach@skolem.tech>")

(package
  (name ppx_expjs)
  (synopsis "Ppx for exporting OCaml functions to be called from Javascript")
  (depends
    (dune (>= 3.0.3))
    (tezt (and (>= 1.0.0) :with-test))
    (ppxlib (>= 0.25.0))))
