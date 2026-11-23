(lang dune 3.24)

(name postgrest)
(generate_opam_files true)
(formatting (enabled_for ocaml))

(license CECILL-B)
(authors "Tristan Brugère")
(maintainers "Tristan Brugère")
(source (github tbrugere/postgrest-ocaml))

(package
 (name postgrest)
 (synopsis "Sans-IO PostgREST client: typed columns, rows, filters, requests")
 (description
  "postgrest builds PostgREST requests and decodes their responses with typed
columns, rows and filters. It performs no IO: plug in any HTTP client.")
 (depends
  (ocaml (>= 5.3))
  (yojson (>= 2.0))
  (ptime (>= 1.0))
  ppx_string
  (uri (>= 4.0))
  (alcotest :with-test)
  (ocamlformat (and :with-dev-setup (= 0.29.0)))))

(package
 (name postgrest-codegen)
 (synopsis "Generates postgrest column declarations from a Postgres catalog")
 (description
  "postgrest-codegen reads the Postgres catalog of a database's tables (the
output of its catalog.sql) and prints the Postgrest.Col declarations of
those tables as OCaml. A build-time tool, separate from the postgrest
library so that the library does not depend on ppxlib.")
 (depends
  (ocaml (>= 5.3))
  (postgrest (= :version))
  (yojson (>= 2.0))
  (ppxlib (>= 0.36))
  (ppx_deriving_yojson (>= 3.10))
  (alcotest :with-test)
  (fmt :with-test)))
