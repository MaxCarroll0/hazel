open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | StandardErrStatus(ErrStatus.t)
  | InconsistentBranches(list(Typ.t), MetaVar.t);
