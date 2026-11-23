# ocaml-postgrest

  Generic, sans-IO PostgREST client for OCaml (`postgrest`), plus a build-time
  code generator (`postgrest-codegen`). See `AI_POLICY.md`.

  ## Who writes what

  - **Human-written, AI must not edit:** the core library, `src/*.ml` and
    `src/*.mli` (top level of `src/`, not `src/codegen/`).
  - **AI may write:** tests (`test/`), build and config files (`dune`,
    `dune-project`, `*.opam`), and the codegen (`src/codegen/`).

  ## Never

  - Never edit, create, delete or reformat a file of the core (`src/*.ml(i)`),
    even for a one-line fix, a typo or a doc comment. Never commit changes
    there.
  - Never run `dune fmt`, `dune build @fmt --auto-promote` or `ocamlformat`
    over the core. Format only the paths you own (`dune build @test/fmt`,
    `@src/codegen/fmt`), and ask first if unsure.
  - Never work around a core limitation from the outside (wrappers, copies,
    re-implementations in tests or codegen). Stop and say what is needed.
  - Never add a dependency to the core library: it stays sans-IO (no HTTP
    client, no Lwt/Eio, no js_of_ocaml/Brr) and generic (no app-specific
    types). The codegen may depend on ppxlib; the core must not.
  - Never hand-write a "recorded" fixture: fixtures in `test/fixtures/` are
    real PostgREST responses, recorded against a real server.
  - Never weaken or delete a test to make it pass.
  - Never put secrets, real data or real coordinates in the repo, and never
    commit with an address other than the GitHub noreply one.
  - Never force-push `main`.

  ## When the core needs a change

  Describe it in chat instead of making it: the file and function, what to
  change, why (the failing case or the missing feature), and a proposed
  signature or snippet. The maintainer writes it.

  ## Conventions

  - `show` / `pp` printers are for tests and logs, never for user-facing text.
  - Errors are values (`result`); the library does not raise.
  - Pull requests are squash-merged; the PR title becomes the commit subject.
