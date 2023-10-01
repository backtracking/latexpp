
# A small LaTeX preprocessor

This is a small tool, quickly implemented, with the following two
purposes:
- be a substitute to LaTeX's package [`listings`](https://ctan.org/pkg/listings)
  in order to avoid the use of `[fragile]` in Beamer slides (slows
  down the compilation)
- insert vertical space that is proportional to the number of empty
  lines in LaTeX source, so that we don't have to use `\bigskip` and
  such all over the place.

There is no documentation apart from the following, non-exhaustive
list of features.

predefined environments:
- math (greek letters, forall, ->, etc.)
- rules (inference rules)
- verbatim
- alltt
- copy: simply copies its argument
- ignore: /dev/null

- tt
- lightblue-tt, lightgreen-tt, lightred-tt

- ocaml-tt
- ocaml-sf
- ocaml (mapped to ocaml-sf)

predefined macros:
- copy: simply copies its argument
- ignore: /dev/null
- math

predefined options:
- color=yes
- vspacing=\medskip
