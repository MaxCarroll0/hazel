open Haz3lcore;
open Util;

// open Haz3lschool;
open Core;

open Specs;
open GradePrelude.Tutorial;
open GradePrelude.GradingT;

[@deriving (sexp, yojson)]
type item = {
  max: int,
  percentage,
  src: string,
};

let item_to_summary = (name, {max, percentage, src}) =>
  Printf.sprintf(
    "%s: %.1f/%.1f\n\n",
    name,
    percentage *. float_of_int(max),
    float_of_int(max),
  )
  ++ (
    if (String.equal(src, "")) {
      "";
    } else {
      "Source Code:\n\n" ++ src ++ "\n\n";
    }
  );

[@deriving (sexp, yojson)]
type report = {
  summary: string,
  overall: score,
};

[@deriving (sexp, yojson)]
type section = {
  name: string,
  report,
};

[@deriving (sexp, yojson)]
type chapter = list(section);

module Main = {
  let settings = CoreSettings.on; /* Statics and Dynamics on */
  let name_to_tutorial_export = path => {
    let yj = Yojson.Safe.from_file(path);
    switch (yj) {
    | `Assoc(l) =>
      let sch = List.Assoc.find_exn(~equal=String.(==), l, "school");
      switch (sch) {
      | `String(sch) =>
        let exercise_export = sch |> deserialize_exercise_export;
        {
          cur_exercise: exercise_export.cur_exercise,
          exercise_data: exercise_export.exercise_data // Ensure this is list((key, persistent_state))
        };
      | _ => failwith("School is not a string")
      };
    | _ => failwith("Json without school key")
    };
  };
  let gen_grading_report = exercise => {
    let zipper_pp = zipper => {
      Printer.pretty_print(zipper);
    };
    let model_results =
      spliced_elabs(settings, exercise)
      |> ModelResults.init_eval
      |> ModelResults.run_pending(~settings);
    let stitched_dynamics =
      stitch_dynamic(settings, exercise, Some(model_results));
    let grading_report = exercise.eds |> GradingReport.mk(~stitched_dynamics);
    let details = grading_report;

    let impl_grading = {
      max: 100, // Set fixed maximum score
      src: exercise.eds.your_impl.state.zipper |> zipper_pp,
      percentage: ImplGradingReport.percentage(details.impl_grading_report),
    };

    let overall = grading_report |> GradingReport.overall_score;
    let (a, b) = overall;
    let summary =
      Printf.sprintf("Overall: %.1f/%.1f\n\n", a, b)
      ++ item_to_summary("Impl Grading", impl_grading);
    {summary, overall};
  };

  let create_section = (item): section => {
    // Separate `key` and `persistent_state` within the function
    let key = fst(item);
    let persistent_state = snd(item);

    switch (find_key_opt(key, specs)) {
    | Some((name, spec)) =>
      // Unpersist the state for the exercise
      let exercise =
        unpersist_state(
          persistent_state,
          ~settings,
          ~spec,
          ~instructor_mode=true,
        );

      // Generate the grading report for this exercise
      let report = gen_grading_report(exercise);

      // Return a `section` record
      {name, report};
    | None => failwith("Invalid spec")
    };
  };

  let run = () => {
    let hw_path = Sys.get_argv()[1];
    let hw = name_to_tutorial_export(hw_path);

    let export_chapter: list(section) =
      List.map(~f=item => create_section(item), hw.exercise_data);

    export_chapter
    |> yojson_of_chapter
    |> Yojson.Safe.pretty_to_string
    |> print_endline;
  };
};

Main.run();
