(lang dune 3.20)

(name xtr)

(generate_opam_files true)

(source
 (github xiaolesu/xtr))

(authors "Xiaole Su")

(maintainers "Xiaole Su")

(license MIT)

(package
 (name xtr)
 (synopsis
  "A typed, modular object-oriented language interpreter with sound linking")
 (description
  "XTR is an interpreter for a typed, modular object-oriented language with
   structural typing (shapes), a CESK abstract machine runtime, and sound
   linking for typed imports. Programs are written as S-expressions and go
   through parsing, validation, type checking, sound linking, and execution.")
 (depends
  (ocaml
   (>= 5.0))
  (core
   (>= 0.17))
  (base
   (>= 0.17))
  (ppx_jane
   (>= 0.17))
  (alcotest :with-test))
 (tags
  (interpreter "programming-languages" ocaml "type-system" cesk)))
