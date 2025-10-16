(lang dune 3.20)

(name tjlb_ocaml)

(generate_opam_files true)

(source
 (github J4n1X/TJLB_ocaml))

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name tjlb_ocaml)
 (synopsis "Programming language compiler in OCaml")
 (description "A simple programming language compiler implemented in OCaml.")
 (depends ocaml ppx_deriving)
)
; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
