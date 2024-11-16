(lang dune 3.16)
(using menhir 2.0)

(name hazel)

(generate_opam_files true)

(source
 (github hazelgrove/hazel))

(authors "Hazel Development Team" "Max Carroll")
(maintainers "Max Carroll")

(license MIT)

(package
 (name hazel)
 (allow_empty)
 (synopsis "Hazel, a live functional programming environment with typed holes")
;  (description "Extended with cast slicing and cast error searching")
 ; (tags
 ; (topics "to describe" your project))
 (depends
  (ocaml
   (>= 5.2.0))
  (menhir
   (>= 2.0))
  yojson
  tezt
  lwt-dllist
  (reason (>= 3.12.0))
  ppx_yojson_conv_lib
  ppx_yojson_conv
  incr_dom
  (omd (<= 1.3.2))
  ppx_deriving
  ptmap
  unionFind
  ocamlformat
  (junit_alcotest :with-test)
  ocaml-lsp-server)) ; After upgrading to opam 2.2 use with-dev https://opam.ocaml.org/blog/opam-2-2-0/

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
