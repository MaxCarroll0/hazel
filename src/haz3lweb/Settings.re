open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type mode =
  | Scratch
  | Documentation
  | Tutorial
  | Exercises;

let mode_of_string = (s: string): mode =>
  switch (s) {
  | "Scratch" => Scratch
  | "Documentation" => Documentation
  | "Tutorial" => Tutorial
  | "Exercises" => Exercises
  | _ => failwith("mode_of_string: unknown mode:" ++ s)
  };

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  captions: bool,
  secondary_icons: bool,
  core: Haz3lcore.CoreSettings.t,
  async_evaluation: bool,
  context_inspector: bool,
  instructor_mode: bool,
  benchmark: bool,
  explainThis: ExplainThisModel.Settings.t,
  mode,
};

let fix_instructor_mode = settings =>
  if (settings.instructor_mode && !ExerciseSettings.show_instructor) {
    {...settings, instructor_mode: false};
  } else {
    settings;
  };
