open Haz3lcore;
open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type hidden_tests('code) = {
  tests: 'code,
  hints: list(string),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type p('code) = {
  title: string,
  description: string,
  hidden_tests: hidden_tests('code),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type state = p(Editor.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type persistent_state = p(PersistentZipper.t);

let scratch_key = n => "scratch_" ++ n;

let persist = (state: state): persistent_state => {
  {
    title: state.title,
    description: state.description,
    hidden_tests: {
      tests: PersistentZipper.persist(state.hidden_tests.tests.state.zipper),
      hints: state.hidden_tests.hints,
    },
  };
};

let unpersist = (zipper: persistent_state, ~settings: CoreSettings.t): state => {
  let editor_zipped = PersistentZipper.unpersist(zipper.hidden_tests.tests);
  let editor = Editor.init(editor_zipped, ~read_only=false, ~settings);
  {
    title: zipper.title,
    description: zipper.description,
    hidden_tests: {
      tests: editor,
      hints: zipper.hidden_tests.hints,
    },
  };
};

let serialize = (state: state): string => {
  persist(state) |> sexp_of_persistent_state |> Sexplib.Sexp.to_string;
};

let deserialize = (data: string, ~settings: CoreSettings.t): state => {
  Sexplib.Sexp.of_string(data)
  |> persistent_state_of_sexp
  |> unpersist(~settings);
};

let deserialize_opt =
    (data: string, ~settings: CoreSettings.t): option(state) => {
  let sexp =
    try(Some(Sexplib.Sexp.of_string(data) |> persistent_state_of_sexp)) {
    | _ => None
    };
  sexp |> Option.map(sexp => sexp |> unpersist(~settings));
};

let export = (state: state): Yojson.Safe.t => {
  state |> persist |> yojson_of_persistent_state;
};

let import = (data: string, ~settings: CoreSettings.t): state => {
  data
  |> Yojson.Safe.from_string
  |> persistent_state_of_yojson
  |> unpersist(~settings);
};

let export_init = (state: state): string => {
  state |> persist |> show_persistent_state;
};
