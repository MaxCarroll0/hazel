open Sexplib.Std;

let export_scratchpad = (state: ScratchSlide.state) => {
  ScratchSlide.persist(state) |> ScratchSlide.yojson_of_persistent_state;
};

let import_scratchpad = data => {
  data
  |> Yojson.Safe.from_string
  |> ScratchSlide.persistent_state_of_yojson
  |> ScratchSlide.unpersist;
};

[@deriving (show({with_path: false}), sexp, yojson)]
type all = {
  settings: string,
  scratch: string,
  school: string,
  log: string,
};

let mk_all = (~instructor_mode) => {
  let settings = LocalStorage.Settings.export();
  let specs = School.exercises;
  {
    settings,
    scratch: LocalStorage.Scratch.export(),
    school: LocalStorage.School.export(~specs, ~instructor_mode),
    log: Log.export(),
  };
};

let export_all = (~instructor_mode) => {
  mk_all(~instructor_mode) |> yojson_of_all;
};

let import_all = (data, ~specs) => {
  let all = data |> Yojson.Safe.from_string |> all_of_yojson;
  let settings = LocalStorage.Settings.import(all.settings); // TODO how does it get into model?
  let instructor_mode = settings.instructor_mode;
  LocalStorage.Scratch.import(all.scratch);
  LocalStorage.School.import(all.school, ~specs, ~instructor_mode);
  Log.import(all.log);
};
