open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = int;

let init = 0;
let equal = (==);

let invalid = (-1);
let is_invalid = equal(invalid);
