open Js_of_ocaml;
open Haz3lcore;
open Sexplib.Std;

let save_settings_key: string = "SETTINGS";
let save_simple_key: string = "SAVE_SIMPLE";
let save_study_key: string = "SAVE_STUDY";
let save_school_key: string = "SAVE_SCHOOL";
let action_log_key: string = "ACTION_LOG";
let keystoke_log_key: string = "KEYSTROKE_LOG";
let zipper_log_key: string = "ZIPPER_LOG";

let set_localstore = (k: string, v: string): unit => {
  let local_store =
    Js.Optdef.get(Dom_html.window##.localStorage, () => assert(false));
  local_store##setItem(Js.string(k), Js.string(v));
};

let get_localstore = (k: string): option(string) =>
  try({
    let local_store =
      Js.Optdef.get(Dom_html.window##.localStorage, () => assert(false));
    local_store##getItem(Js.string(k))
    |> (
      x => Js.Opt.get(x, () => assert(false)) |> Js.to_string |> Option.some
    );
  }) {
  | _ => None
  };

let save_settings = (settings: Model.settings): unit =>
  set_localstore(
    save_settings_key,
    settings |> Model.sexp_of_settings |> Sexplib.Sexp.to_string,
  );

let load_settings = (): Model.settings =>
  switch (get_localstore(save_settings_key)) {
  | None => Model.settings_init
  | Some(flag) =>
    try(flag |> Sexplib.Sexp.of_string |> Model.settings_of_sexp) {
    | _ => Model.settings_init
    }
  };

[@deriving (show({with_path: false}), sexp, yojson)]
type simple_without_history = (Id.t, Zipper.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type study_without_history = (Id.t, int, list(Zipper.t));

[@deriving (show({with_path: false}), sexp, yojson)]
type school_without_history = study_without_history;

let save_simple = (simple: Editors.simple): unit =>
  set_localstore(
    save_simple_key,
    simple
    |> (((id_gen, ed: Editor.t)) => (id_gen, ed.state.zipper))
    |> sexp_of_simple_without_history
    |> Sexplib.Sexp.to_string,
  );

let load_simple = (): Editors.simple =>
  switch (get_localstore(save_simple_key)) {
  | None => Model.simple_init
  | Some(flag) =>
    try(
      flag
      |> Sexplib.Sexp.of_string
      |> simple_without_history_of_sexp
      |> (((id_gen, zipper)) => (id_gen, Editor.init(zipper)))
    ) {
    | _ => Model.simple_init
    }
  };

let trim_histories: list(Editor.t) => list(Zipper.t) =
  List.map((ed: Editor.t) => ed.state.zipper);

let add_histories: list(Zipper.t) => list(Editor.t) = List.map(Editor.init);

let prep_school_in =
    ((id_gen, idx, eds): Editors.school): school_without_history => (
  id_gen,
  idx,
  trim_histories(eds),
);

let prep_school_out =
    ((id_gen, idx, eds): school_without_history): Editors.school => (
  id_gen,
  idx,
  add_histories(eds),
);

let prep_study_in = prep_school_in;

let prep_study_out = prep_school_out;

let save_study = (study: Editors.study): unit =>
  set_localstore(
    save_study_key,
    study
    |> prep_study_in
    |> sexp_of_study_without_history
    |> Sexplib.Sexp.to_string,
  );

let load_study = (): Editors.study =>
  switch (get_localstore(save_study_key)) {
  | None => Study.init
  | Some(flag) =>
    try(
      flag
      |> Sexplib.Sexp.of_string
      |> study_without_history_of_sexp
      |> prep_study_out
    ) {
    | _ => Study.init
    }
  };

let save_school = (school: Editors.school): unit =>
  set_localstore(
    save_school_key,
    school
    |> prep_school_in
    |> sexp_of_school_without_history
    |> Sexplib.Sexp.to_string,
  );

let load_school = (): Editors.school =>
  switch (get_localstore(save_school_key)) {
  | None => School.init
  | Some(flag) =>
    try(
      flag
      |> Sexplib.Sexp.of_string
      |> school_without_history_of_sexp
      |> prep_school_out
    ) {
    | _ => School.init
    }
  };
