[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Singleton(Typ.t)
  | Abstract;
