open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = string;
