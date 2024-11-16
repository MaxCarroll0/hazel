[@deriving (show({with_path: false}), sexp, of_yojson, yojson_of)]
type t = int;
let eq: (t, t) => bool;
