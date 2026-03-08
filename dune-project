(lang dune 3.21)

(name P4)

(generate_opam_files true)

(source
 (github P4-Group2-Compiler/P4-State-Lang))

(authors "P4 Group 2")
(maintainers "P4 Group 2")

(license MIT)

(documentation https://github.com/P4-Group2-Compiler/P4-State-Lang)

(package
 (name P4)
 (synopsis "State-based DSL compiler")
 (description "A small compiler/interpreter for a state-based DSL.")
 (depends
  (ocaml (>= 4.14))
  dune
  alcotest)
 (tags ("compiler" "dsl" "state-machine")))
; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
