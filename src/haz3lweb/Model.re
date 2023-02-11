open Sexplib.Std;
open Haz3lcore;

[@deriving (show({with_path: false}), yojson, sexp)]
type timestamp = float;

[@deriving (show({with_path: false}), sexp, yojson)]
type live_inspector = {
  on: bool,
  use_cursor: bool,
  show_fns_in_env: bool,
  ids: list(Id.t),
  cur_env_idx: int,
  cur_env: ProbeMap.dhexp_env,
};

let live_inspector_init = {
  on: false,
  use_cursor: true,
  show_fns_in_env: false,
  ids: [],
  cur_env_idx: 0,
  cur_env: [],
};

[@deriving (show({with_path: false}), sexp, yojson)]
type settings = {
  live_inspector,
  captions: bool,
  secondary_icons: bool,
  statics: bool,
  dynamics: bool,
  async_evaluation: bool,
  context_inspector: bool,
  instructor_mode: bool,
  benchmark: bool,
  mode: Editors.mode,
};

let settings_init = {
  live_inspector: live_inspector_init,
  captions: true,
  secondary_icons: false,
  statics: true,
  dynamics: true,
  async_evaluation: false,
  context_inspector: false,
  instructor_mode: SchoolSettings.show_instructor,
  benchmark: false,
  mode: Editors.Scratch,
};

let settings_debug = {...settings_init, mode: Editors.DebugLoad};

let fix_instructor_mode = settings =>
  if (settings.instructor_mode && !SchoolSettings.show_instructor) {
    {...settings, instructor_mode: false};
  } else {
    settings;
  };

type t = {
  editors: Editors.t,
  results: ModelResults.t,
  settings,
  font_metrics: FontMetrics.t,
  logo_font_metrics: FontMetrics.t,
  show_backpack_targets: bool,
  double_tap: option(timestamp),
  mousedown: bool,
  langDocMessages: LangDocMessages.t,
};

let cutoff = (===);

let mk = editors => {
  editors,
  results: ModelResults.empty,
  settings: settings_init,
  // TODO: move below to 'io_state'?
  font_metrics: FontMetrics.init,
  logo_font_metrics: FontMetrics.init,
  show_backpack_targets: false,
  double_tap: None,
  mousedown: false,
  langDocMessages: LangDocMessages.init,
};

let blank = mk(Editors.Scratch(0, []));
let debug = mk(Editors.DebugLoad);
