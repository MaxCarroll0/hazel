open Virtual_dom.Vdom;
open Node;
open Core;

/*
 let res_view = (term, info_map) => {
   let res_text =
     switch (get_result(term, info_map)) {
     | Some(d) => Sexplib.Sexp.to_string_hum(DHExp.sexp_of_t(d))
     | None => "NO_RESULT"
     | exception _ => "EXCEPTION"
     };
   div([Attr.classes(["result"])], [text("RES:" ++ res_text)]);
 };
 */

let ci_view = (zipper: Zipper.t, info_map) => {
  let index =
    switch (zipper |> Indicated.index) {
    | Some(index) => index
    | None => (-1)
    };
  let ci =
    switch (zipper |> Indicated.index) {
    | Some(index) => Id.Map.find_opt(index, info_map)
    | None => None
    };
  switch (ci) {
  | None => div([], [])
  | Some(ci) => CursorInspector.view(index, ci)
  };
};

type fat_zipper = {
  zipper: Zipper.t,
  tile_map: Measured.t,
  segment: Segment.t,
  unselected: Segment.t,
  term: Term.UExp.t,
  info_map: Statics.info_map,
};

let fat_zipper = (zipper: Zipper.t) => {
  let term = zipper |> Term.of_zipper;
  let (_, _, info_map) = term |> Statics.uexp_to_info_map;
  let segment = Zipper.zip(zipper);
  let unselected = Zipper.unselect_and_zip(zipper);
  let tile_map = Measured.of_segment(unselected);
  {zipper, term, info_map, segment, unselected, tile_map};
};

let deco = (~fat_zipper, ~font_metrics, show_backpack_targets) => {
  module Deco =
    Deco.Deco({
      let font_metrics = font_metrics;
      let map = fat_zipper.tile_map;
      let show_backpack_targets = show_backpack_targets;
    });
  Deco.all(fat_zipper.zipper, fat_zipper.segment);
};

let code_container =
    (
      ~font_metrics,
      ~fat_zipper: fat_zipper,
      ~settings,
      ~show_backpack_targets,
    ) => {
  div(
    [Attr.class_("code-container")],
    [
      Code.view(
        ~font_metrics,
        ~sel_seg=fat_zipper.segment,
        ~unsel_seg=fat_zipper.unselected,
        ~map=fat_zipper.tile_map,
        ~settings,
      ),
    ]
    @ deco(~fat_zipper, ~font_metrics, show_backpack_targets),
  );
};

let single_view =
    (
      ~font_metrics,
      ~show_backpack_targets,
      ~zipper: Zipper.t,
      ~settings: Model.settings,
    )
    : Node.t => {
  let fat_zipper = fat_zipper(zipper);
  div(
    [Attr.class_("editor")],
    [
      code_container(
        ~font_metrics,
        ~fat_zipper,
        ~settings,
        ~show_backpack_targets,
      ),
      ci_view(fat_zipper.zipper, fat_zipper.info_map),
      TestView.view(
        ~font_metrics,
        Elaborator.uexp_elab(fat_zipper.info_map, fat_zipper.term),
      ),
      Interface.res_view(~font_metrics, fat_zipper.term, fat_zipper.info_map),
    ],
  );
};

let column_view =
    (
      ~font_metrics,
      ~show_backpack_targets,
      ~stages: list(Model.stage),
      ~selected as _,
      ~settings,
      ~focal_zipper: Zipper.t,
    ) => {
  let fat_zippers =
    List.map((stage: Model.stage) => fat_zipper(stage.z), stages);
  let fat_zipper = fat_zipper(focal_zipper);
  //TODO(andrew): now these just point at selected one
  div(
    [Attr.classes(["editor", "column"])],
    List.map(
      fz =>
        div(
          [Attr.class_("cell")],
          [
            code_container(
              ~font_metrics,
              ~fat_zipper=fz,
              ~settings,
              ~show_backpack_targets,
            ),
          ],
        ),
      fat_zippers,
    )
    @ [
      ci_view(fat_zipper.zipper, fat_zipper.info_map),
      TestView.view(
        ~font_metrics,
        Elaborator.uexp_elab(fat_zipper.info_map, fat_zipper.term),
      ),
      //text(string_of_int(selected)),
      Interface.res_view(~font_metrics, fat_zipper.term, fat_zipper.info_map),
    ],
  );
};

let view =
    (
      ~font_metrics,
      ~show_backpack_targets,
      ~settings: Model.settings,
      ~editor_model: Model.editor_model,
    )
    : Node.t => {
  let focal_zipper = Model.get_zipper'(editor_model);
  switch (editor_model) {
  | Simple(_)
  | Study(_) =>
    single_view(
      ~font_metrics,
      ~show_backpack_targets,
      ~zipper=focal_zipper,
      ~settings,
    )
  | School(selected, stages) =>
    column_view(
      ~font_metrics,
      ~show_backpack_targets,
      ~stages,
      ~selected,
      ~settings,
      ~focal_zipper,
    )
  };
};
