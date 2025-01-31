open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = int;

// TODO: distinguish types of holes:
// 1) user-inserted holes
// 2) internal holes:
//    a) static errors
//    b) dynamic errors
//    c) other inserted holes??
