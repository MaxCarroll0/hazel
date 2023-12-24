open Util.OptUtil.Syntax;
open Virtual_dom.Vdom;
open Haz3lcore;

let get_suggestion_ui_for_id =
    (
      ~font_metrics,
      id: Id.t,
      global_inference_info: InferenceResult.global_inference_info,
      colored_ui: bool,
    )
    : InferenceResult.suggestion(Node.t) =>
  if (global_inference_info.enabled) {
    print_endline("in get suggestion ui for id " ++ Id.to_string(id));
    let status_opt =
      Hashtbl.find_opt(global_inference_info.solution_statuses, id);
    switch (status_opt) {
    | Some(Solved(Unknown(_))) =>
      print_endline("no suggestion only holes");
      NoSuggestion(OnlyHoleSolutions);
    | Some(Solved(ityp)) =>
      print_endline("suggestion solved as a single type");
      Solvable(
        ityp
        |> ITyp.ityp_to_typ
        |> Type.view(~font_metrics=Some(font_metrics), ~with_cls=false),
      );
    | Some(Unsolved([potential_typ])) =>
      print_endline("Suggestion unsolved as a single type");
      let ptyp_node =
        Type.view_of_potential_typ(
          ~font_metrics,
          ~with_cls=colored_ui,
          false,
          potential_typ,
        );
      NestedInconsistency(ptyp_node);
    | Some(Unsolved(_)) =>
      print_endline("No sugguestion unsolved as many");
      NoSuggestion(InconsistentSet);
    | None =>
      print_endline("No Suggestion non type hole id");
      NoSuggestion(NonTypeHoleId);
    };
  } else {
    print_endline("in get suggestion ui");
    print_endline("no suggestion disabled");
    NoSuggestion(SuggestionsDisabled);
  };

let svg_display_settings =
    (~global_inference_info: InferenceResult.global_inference_info, id: Id.t)
    : (bool, bool) => {
  // Determines if a hexagon (svg) should be used to represent a type hole, and if so, how it should look
  print_endline("calling get suggestion text from svg display settingss");
  let (show_svg, is_unsolved) =
    switch (
      InferenceResult.get_suggestion_text_for_id(id, global_inference_info)
    ) {
    | Solvable(_) => (false, false)
    | NestedInconsistency(_) => (false, true)
    | NoSuggestion(SuggestionsDisabled)
    | NoSuggestion(OnlyHoleSolutions) => (true, false)
    | NoSuggestion(NonTypeHoleId) => (true, false)
    | NoSuggestion(InconsistentSet) => (true, true)
    };
  (show_svg, is_unsolved);
};

let get_cursor_inspect_result =
    (~global_inference_info: InferenceResult.global_inference_info, id: Id.t)
    : option((bool, list(Typ.t))) =>
  if (global_inference_info.enabled) {
    let* status =
      Hashtbl.find_opt(global_inference_info.solution_statuses, id);
    switch (status) {
    | Unsolved(potential_typ_set) =>
      Some((
        false,
        potential_typ_set
        |> PotentialTypeSet.potential_typ_set_to_ityp_unroll(id)
        |> List.map(ITyp.ityp_to_typ),
      ))
    | Solved(ityp) => Some((true, [ityp |> ITyp.ityp_to_typ]))
    };
  } else {
    None;
  };
