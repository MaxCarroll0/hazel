open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  captions: bool,
  secondary_icons: bool,
  statics: bool,
  dynamics: bool,
  async_evaluation: bool,
  context_inspector: bool,
  benchmark: bool,
  mode: Editors.mode,
};

let default = {
  captions: true,
  secondary_icons: false,
  statics: true,
  dynamics: true,
  async_evaluation: false,
  context_inspector: false,
  benchmark: false,
  mode: Editors.Scratch,
};
let init_debug = {...default, mode: Editors.DebugLoad};

let key: string = "SETTINGS";
let serialize = s => s |> sexp_of_t |> Sexplib.Sexp.to_string;
let deserialize = s => s |> Sexplib.Sexp.of_string |> t_of_sexp;
