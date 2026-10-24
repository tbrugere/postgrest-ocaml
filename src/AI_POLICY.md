# AI policy for ocaml-postgrest

The core part of the library is human coded (while questions were asked to AI, no code was written by AI).

Parts of the code that were written by AI are:
- the tests
- build and config files (opam files, dune files…)
- the codegen module.

(ie the annoying stuff)

So part of the repo is AI-written, *but* if you import ocaml-postgrest, **you are not importing any ai-written code**. 

*Caveat*: If you use the codegen you could say your code is indirectly ai-written. If that bothers you, feel free to contribute a human-written codegen module.
