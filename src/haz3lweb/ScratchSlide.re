open Sexplib.Std;
open Haz3lcore;
<<<<<<< Updated upstream
// open SyntaxTest;
=======
open Util;
>>>>>>> Stashed changes

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
<<<<<<< Updated upstream

[@deriving (show({with_path: false}), sexp, yojson)]
type hint = string;

// [@deriving (show({with_path: false}), sexp, yojson)]
// type syntax_test = (hint, SyntaxTest.predicate);

// [@deriving (show({with_path: false}), sexp, yojson)]
// type syntax_tests = list(syntax_test);

[@deriving (show({with_path: false}), sexp, yojson)]
type your_tests('code) = {
  tests: 'code,
  required: int,
  provided: int,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type point_distribution = {
  test_validation: int,
  mutation_testing: int,
  impl_grading: int,
};

let validate_point_distribution =
    ({test_validation, mutation_testing, impl_grading}: point_distribution) =>
  test_validation + mutation_testing + impl_grading == 100
    ? () : failwith("Invalid point distribution in exercise.");

// why are neither of these functions working?
let toEditor = (state: state): Editor.t => {
  switch (state) {
  | s => s.hidden_tests.tests
  };
};

let fromEditor = (editor: Editor.t): state => {
  title: "",
  description: "",
  hidden_tests: {
    tests: editor,
    hints: [],
  },
};
=======
>>>>>>> Stashed changes

let scratch_key = n => "scratch_" ++ n;

let persist = (editor: Editor.t) => {
  PersistentZipper.persist(editor.state.zipper);
};

<<<<<<< Updated upstream
// let persist = (editor: p(Editor.t)) => {
//   let zip = editor.hidden_tests.tests.state.zipper;
//   PersistentZipper.persist(zip);
// };

let unpersist = (zipper: persistent_state) => {
  let zipper = PersistentZipper.unpersist(zipper.hidden_tests.tests);
  Editor.init(zipper, ~read_only=false);
};

// let unpersist = (zipper: persistent_state) => {
//   let zipper = PersistentZipper.unpersist(zipper.hidden_tests.tests);
//   Editor.init(zipper, ~read_only=false);
// };

=======
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

>>>>>>> Stashed changes
let serialize = (state: state) => {
  let editor = persist(state.hidden_tests.tests);
  let persistent_state: persistent_state = {
    title: state.title,
    description: state.description,
    hidden_tests: {
      tests: editor,
      hints: state.hidden_tests.hints,
    },
  };
<<<<<<< Updated upstream
  // Sexplib.Sexp.to_string (sexp_of_persistent_state persistent_state)
  persistent_state |> sexp_of_persistent_state |> Sexplib.Sexp.to_string;
  // Persist(editor) |> sexp_of_persistent_state |> Sexplib.Sexp.to_string;
=======
  persistent_state |> sexp_of_persistent_state |> Sexplib.Sexp.to_string;
>>>>>>> Stashed changes
};

let deserialize = (data: string) => {
  Sexplib.Sexp.of_string(data) |> persistent_state_of_sexp |> unpersist;
};

let deserialize_opt = (data: string) => {
  let sexp =
    try(Some(Sexplib.Sexp.of_string(data) |> persistent_state_of_sexp)) {
    | _ => None
    };
  sexp |> Option.map(sexp => sexp |> unpersist);
};

let export = (state: state) => {
  let editor = persist(state.hidden_tests.tests);
  let persistent_state: persistent_state = {
    title: state.title,
    description: state.description,
    hidden_tests: {
      tests: editor,
      hints: state.hidden_tests.hints,
    },
  };
  persistent_state |> yojson_of_persistent_state;
};

// let export = (state: persistent_state) => {
//   state |> yojson_of_persistent_state;
// };

let import = (data: string) => {
  data |> Yojson.Safe.from_string |> persistent_state_of_yojson |> unpersist;
};

let export_init = (state: state) => {
  let editor = persist(state.hidden_tests.tests);
  let persistent_state: persistent_state = {
    title: state.title,
    description: state.description,
    hidden_tests: {
      tests: editor,
      hints: state.hidden_tests.hints,
    },
  };
  persistent_state |> show_persistent_state;
<<<<<<< Updated upstream
};

// let export_init = (state: persistent_state) => {
//   state |> show_persistent_state;
// };

let mk_statics =
    (~settings: Settings.t, editor: Editor.t, ctx_init: Ctx.t)
    : CachedStatics.statics => {
  let term = MakeTerm.from_zip_for_sem(editor.state.zipper) |> fst;
  let info_map = Interface.Statics.mk_map_ctx(settings.core, ctx_init, term);
  let error_ids =
    Statics.Map.error_ids(editor.state.meta.term_ranges, info_map);
  {term, info_map, error_ids};
=======
>>>>>>> Stashed changes
};
