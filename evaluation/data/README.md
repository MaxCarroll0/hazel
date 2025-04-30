# Source Data
## Ill-Typed

This data is sourced from OCaml student interactions in a course at UC San Diego, collected by Eric L Seidel and Ranjit Jhala. The data, [A Collection of Novice Interactions with the OCaml Top-Level System], is freely available for use and translation under a [CC0] licence.
[CC0]: https://creativecommons.org/publicdomain/zero/1.0/
[A Collection of Novice Interactions with the OCaml Top-Level System]: https://doi.org/10.5281/zenodo.806813

Any publications using this original and our derived data should cite this dataset:
``` bibtex
@misc{yunounderstand,
  author       = {Eric L Seidel and Ranjit Jhala},
  title        = {A Collection of Novice Interactions with the {OCaml} {Top-Level} System},
  month        = jun,
  year         = 2017,
  doi          = {10.5281/zenodo.806814},
  url          = {https://doi.org/10.5281/zenodo.806814}
}
```
We considered their source derived minimal OCaml programs, [derived/comb/progs]. Taking _both_ constructor and unification errors.
[derived/comb/progs]: https://github.com/ucsd-progsys/yunounderstand-data/tree/4321f31638d3d8e8e9710dcf6ddb32929ab649bf/data/derived/comb/progs

## Well-Typed
TODO: OCaml standard library. Licensing...

# Derived Data

The resulting Hazel corpus can be found separately at [hazel-corpus], and was translated to OCaml via [hazel-of-ocaml], producing both dynamically typed and fully or partially typed (`.typed`) equivalents.
[hazel-corpus]: https://github.com/patricoferris/hazel-corpus/tree/main
[hazel-of-ocaml]: https://github.com/patricoferris/hazel_of_ocaml

Programs without equivalent Hazel builtin functions or operators were omitted.

This project additionally applies holes to each function defined by the ill-typed program. This is the form in which a dynamic witness search procedure, [witnesses-search], can be performed upon the ill-typed programs. For example:
```let sumList = fun xs -> case xs 
  | [] => []
  | h1 :: h2 :: t => h1 + h2(sumList)(t)
end in ?```
Is transformed to:
```let sumList = fun xs -> case xs 
  | [] => []
  | h1 :: h2 :: t => h1 + h2(sumList)(t)
end in sumList(?)```

