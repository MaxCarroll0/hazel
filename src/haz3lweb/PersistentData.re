open Sexplib.Std;
open Haz3lcore;

[@deriving (show({with_path: false}), sexp, yojson)]
type scratch = (
  int,
  list(ScratchSlide.persistent_state),
  list((string, ModelResult.persistent)),
);

[@deriving (show({with_path: false}), sexp, yojson)]
type documentation = (
  string,
  list((string, ScratchSlide.persistent_state)),
  [@default []] list((string, ModelResult.persistent)),
);

[@deriving (show({with_path: false}), sexp, yojson)]
type tutorial = (
  string,
  list((string, DocumentationEnv.persistent_state)),
  [@default []] list((string, ModelResult.persistent)),
);

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  settings: Settings.t,
  scratch,
  documentation,
  tutorial,
};
