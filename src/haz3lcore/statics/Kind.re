[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Singleton(TypBase.t)
  | Abstract;
