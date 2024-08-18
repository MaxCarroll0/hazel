open Virtual_dom.Vdom;
open Js_of_ocaml;
open Haz3lcore;
open Node;
open Util;

open Haz3lschool.ProofGrade.F(Exercise.ExerciseEnv);
open Exercise.Proof;

type view_info = (pos, VerifiedTree.res, ed)
and ed =
  | Just(Derivation.Rule.t, Editor.t, Exercise.DynamicsItem.t)
  | Abbr(index);

let proof_view =
    (
      ~inject: UpdateAction.t => Ui_effect.t(unit),
      ~ui_state: Model.ui_state,
      ~settings: Settings.t,
      ~pos: pos,
      ~grading_report: GradingReport.t,
      ~eds: model(Editor.t),
      ~stitched_dynamics: stitched(Exercise.DynamicsItem.t),
      ~highlights,
    ) => {
  let init_editor = () =>
    "" |> Exercise.zipper_of_code |> Editor.init(~settings=settings.core);

  let map_model = (f, state: Exercise.state): Exercise.state => {
    ...state,
    model:
      switch (state.model) {
      | Proof(m) => Proof(f(m))
      | _ => raise(Failure("Expected Exercise.Proof"))
      },
  };

  let switch_rule =
      (m: model(Editor.t), ~pos: pos, ~rule: Derivation.Rule.t)
      : model(Editor.t) =>
    switch (pos) {
    | Prelude => failwith("ProofAction.switch_rule: Prelude")
    | Trees(i, pos) =>
      let node = m.trees |> List.nth(_, i) |> Tree.nth(_, pos);
      let jdmt =
        switch (node) {
        | Just({jdmt, _}) => jdmt
        | Abbr(_) => init_editor()
        };
      let tree =
        Tree.put_nth(
          Exercise.Proof.Just({jdmt, rule}),
          List.nth(m.trees, i),
          pos,
        );
      let trees = ListUtil.put_nth(i, tree, m.trees);
      {...m, trees};
    };

  let dropdown_option_rule_view = (~pos: pos, ~rule: Derivation.Rule.t) =>
    div(
      ~attrs=[
        Attr.class_("dropdown-option"),
        Attr.class_("rule"),
        Attr.on_click(_ =>
          inject(
            UpdateAction.MapExercise(map_model(switch_rule(~pos, ~rule))),
          )
        ),
      ],
      [text(Derivation.Rule.repr(rule))],
    );

  let switch_abbr = (m: model(Editor.t), ~pos: pos, ~index: index) =>
    switch (pos) {
    | Prelude => failwith("ProofAction.switch_abbr: Prelude")
    | Trees(i, pos) =>
      let tree =
        Tree.put_nth(Exercise.Proof.Abbr(index), List.nth(m.trees, i), pos);
      let trees = ListUtil.put_nth(i, tree, m.trees);
      {...m, trees};
    };

  let dropdown_option_abbr_view = (~pos: pos, ~index: index) =>
    div(
      ~attrs=[
        Attr.class_("dropdown-option"),
        Attr.class_("abbr"),
        Attr.on_click(_ =>
          inject(
            UpdateAction.MapExercise(map_model(switch_abbr(~pos, ~index))),
          )
        ),
      ],
      [text(string_of_int(index))],
    );

  let abbr_num: pos => index =
    fun
    | Prelude => failwith("ProofView.abbr_num: Prelude")
    | Trees(i, _) => i;

  let dropdown_option_container_view = (~pos): t =>
    div(
      ~attrs=[Attr.class_("dropdown-option-container")],
      List.map(
        rule => dropdown_option_rule_view(~pos, ~rule),
        Derivation.Rule.all,
      )
      @ List.map(
          index => dropdown_option_abbr_view(~pos, ~index),
          List.init(abbr_num(pos), Fun.id),
        ),
    );

  let dropdown_result: VerifiedTree.res => string =
    fun
    | Error(_) => "⌛️"
    | Ok({err: Some(err), _}) => "❌" ++ DerivationError.repr(err)
    | Ok(_) => "✅";

  let dropdown_result_view = (~res): t =>
    div(
      ~attrs=[Attr.class_("dropdown-result")],
      [text(dropdown_result(res))],
    );

  let class_of_result: VerifiedTree.res => string =
    fun
    | Ok({err: Some(_), _}) => "incorrect"
    | Ok(_) => "correct"
    | Error(_) => "pending";

  let dropdown_view = (~pos, ~res: VerifiedTree.res): t =>
    div(
      ~attrs=[
        Attr.class_("dropdown"),
        Attr.id(show_pos(pos) ++ "-content"),
        Attr.class_(class_of_result(res)),
      ],
      [dropdown_result_view(~res), dropdown_option_container_view(~pos)],
    );

  let label_on_mouseover = (~pos) => {
    let show_pos = show_pos(pos);
    let label = Util.JsUtil.get_elem_by_id(show_pos ++ "-label");
    let content = Util.JsUtil.get_elem_by_id(show_pos ++ "-content");
    let label_rect = label##getBoundingClientRect;
    let content_rect = content##getBoundingClientRect;
    let content_height = Js.Optdef.get(content_rect##.height, Fun.const(0.));
    let content_width = Js.Optdef.get(content_rect##.width, Fun.const(0.));
    let window_width = float_of_int(Dom_html.window##.innerWidth);
    let top = label_rect##.top -. content_height;
    let left = min(label_rect##.left, window_width -. content_width);
    let style = Js.string(Printf.sprintf("top:%fpx; left:%fpx;", top, left));
    content##setAttribute(Js.string("style"), style); // This may still cause the content to overflow the window
    Ui_effect.Ignore;
  };

  let label_view = (~pos, ~res, ~label) =>
    div(
      ~attrs=[
        Attr.class_("deduction-label"),
        Attr.class_(class_of_result(res)),
        Attr.id(show_pos(pos) ++ "-label"),
        Attr.on_mousemove(_ => label_on_mouseover(~pos)),
      ],
      [text(label)],
    );

  let label_view = (~pos, ~res, ~label): t =>
    div(
      ~attrs=[Attr.class_("deduction-label-container")],
      [label_view(~pos, ~res, ~label), dropdown_view(~pos, ~res)],
    );

  let add_premise = (m: model(Editor.t), ~pos, ~index): model(Editor.t) =>
    switch (pos) {
    | Prelude => failwith("ProofAction.add_premise: Prelude")
    | Trees(i, pos) =>
      let jdmt =
        Exercise.Proof.Just({
          jdmt: init_editor(),
          rule: Derivation.Rule.Assumption,
        });
      let tree = Tree.insert(jdmt, index, List.nth(m.trees, i), pos);
      {...m, trees: ListUtil.put_nth(i, tree, m.trees)};
    };

  let add_premise_btn_view = (~pos, ~index) =>
    div(
      ~attrs=[
        Attr.class_("add-premise-btn"),
        Attr.on_click(_ =>
          inject(
            UpdateAction.MapExercise(map_model(add_premise(~pos, ~index))),
          )
        ),
      ],
      [text("+")],
    );

  let del_premise = (m: model(Editor.t), ~pos, ~index): model(Editor.t) =>
    switch (pos) {
    | Prelude => failwith("ProofAction.del_premise: Prelude")
    | Trees(i, pos) =>
      let (_, tree) = Tree.remove(index, List.nth(m.trees, i), pos);
      {...m, trees: ListUtil.put_nth(i, tree, m.trees)};
    };

  let del_premise_pos_check = (state: Exercise.state, ~pos, ~index) => {
    ...state,
    pos:
      switch (pos) {
      | Prelude => state.pos // unreachable
      | Trees(i, pos) =>
        let pos': Exercise.pos =
          Proof(Trees(i, Tree.pos_concat(Children(index, Value), pos)));
        pos' == state.pos ? Proof(Trees(i, pos)) : state.pos;
      },
  };

  let del_premise_btn_view = (~pos, ~index) =>
    div(
      ~attrs=[
        Attr.class_("add-premise-btn"),
        Attr.on_click(_ =>
          inject(
            UpdateAction.MapExercise(
              state =>
                state
                |> map_model(del_premise(~pos, ~index))
                |> del_premise_pos_check(~pos, ~index),
            ),
          )
        ),
      ],
      [text("-")],
    );

  let premises_view = (~children_node, ~pos, ~res, ~rule) => {
    let n = List.length(children_node);
    let label = Derivation.Rule.repr(rule);
    div(
      ~attrs=[
        Attr.class_("deduction-prems"),
        Attr.class_(class_of_result(res)),
      ],
      (
        children_node
        |> List.mapi((index, node) =>
             [
               add_premise_btn_view(~pos, ~index),
               node,
               del_premise_btn_view(~pos, ~index),
             ]
           )
        |> List.concat
      )
      @ [
        add_premise_btn_view(~pos, ~index=n),
        label_view(~pos, ~res, ~label),
      ],
    );
  };

  let editor_view =
      (this_pos, ~editor, ~di: Exercise.DynamicsItem.t, ~caption) =>
    Cell.editor_view(
      ~selected=(Proof(pos): Exercise.pos) == this_pos,
      ~override_statics=di.statics,
      ~inject,
      ~ui_state,
      ~mousedown_updates=[SwitchEditor(this_pos)],
      ~settings,
      ~highlights,
      ~caption,
      ~target_id=Exercise.show_pos(this_pos),
      ~test_results=ModelResult.test_results(di.result),
      editor,
    );

  let conclusion_view = (~pos, ~editor, ~di) =>
    div(
      ~attrs=[Attr.class_("deduction-concl")],
      [editor_view(Proof(pos), ~editor, ~di, ~caption=None)],
    );

  let deduction_view = (~children_node, ~pos, ~res, ~rule, ~editor, ~di) =>
    div(
      ~attrs=[Attr.class_("deduction-just")],
      [
        premises_view(~children_node, ~pos, ~res, ~rule),
        conclusion_view(~pos, ~editor, ~di),
      ],
    );

  // TODO: Refactor this
  let abbreviation_view = (~pos, ~res, ~index) =>
    div(
      ~attrs=[Attr.class_("deduction-abbr")],
      [
        div(
          ~attrs=[
            Attr.class_("deduction-prems"),
            Attr.class_(class_of_result(res)),
          ],
          [label_view(~pos, ~res, ~label="")],
        ),
        div(
          ~attrs=[Attr.class_("deduction-concl")],
          [
            div(
              ~attrs=[Attr.class_("code")],
              [
                span(
                  ~attrs=[Attr.class_("code-text")],
                  [
                    span(
                      ~attrs=[Attr.class_("token default Pat poly")],
                      [text("d" ++ string_of_int(index))],
                    ),
                  ],
                ),
              ],
            ),
          ],
        ),
      ],
    );

  let deduction_view = ((pos, res, ed): view_info, children_node: list(t)) =>
    switch (ed) {
    | Just(rule, editor, di) =>
      deduction_view(~children_node, ~pos, ~res, ~rule, ~editor, ~di)
    | Abbr(index) => abbreviation_view(~pos, ~res, ~index)
    };

  let abbreviation_wrapper = (i, t) => {
    let token_wrapper = (cls, s) =>
      span(~attrs=[Attr.class_(cls)], [text(s)]);
    let span_exp = token_wrapper("token default Exp poly");
    let span_pat = token_wrapper("token default Pat mono");
    let span_secondary = token_wrapper("secondary");
    let code_wrapper = code =>
      div(
        ~attrs=[Attr.class_("code")],
        [span(~attrs=[Attr.class_("code-text")], code)],
      );
    let upper_code =
      [
        span_exp("let"),
        span_secondary(" "),
        span_pat("d" ++ string_of_int(i)),
        span_secondary(" "),
        span_exp("="),
      ]
      |> code_wrapper;
    let lower_code = [span_exp("in")] |> code_wrapper;
    div(~attrs=[], [upper_code, t, lower_code]);
  };

  let add_abbr = (m: model(Editor.t), ~index): model(Editor.t) => {
    let trees =
      m.trees
      |> List.mapi(j =>
           if (j >= index) {
             Tree.map(
               fun
               | Exercise.Proof.Just(_) as j => j
               | Abbr(i) => i >= index ? Abbr(i + 1) : Abbr(i),
             );
           } else {
             Fun.id;
           }
         );
    {...m, trees};
  };

  let add_abbr_btn_view = (~index) =>
    div(
      ~attrs=[
        Attr.class_("add-abbr-btn"),
        Attr.on_click(_ =>
          inject(UpdateAction.MapExercise(map_model(add_abbr(~index))))
        ),
      ],
      [text("+")],
    );
  ignore(add_abbr_btn_view);

  let del_abbr = (m: model(Editor.t), ~index): model(Editor.t) => {
    let trees =
      m.trees
      |> List.mapi(j =>
           if (j >= index) {
             Tree.map(
               fun
               | Exercise.Proof.Just(_) as j => j
               | Abbr(i) => {
                   // TODO: consider when it refer to the deleted abbr
                   i > index
                     ? Abbr(i - 1) : Abbr(i);
                 },
             );
           } else {
             Fun.id;
           }
         );
    {...m, trees};
  };

  // TODO: implement del_abbr_pos_check
  let del_abbr_pos_check = (state: Exercise.state, ~index) => {
    ...state,
    pos: {
      ignore(index);
      state.pos;
    },
  };

  let del_abbr_btn_view = (~index) =>
    div(
      ~attrs=[
        Attr.class_("del-abbr-btn"),
        Attr.on_click(_ =>
          inject(
            UpdateAction.MapExercise(
              state =>
                state
                |> map_model(del_abbr(~index))
                |> del_abbr_pos_check(~index),
            ),
          )
        ),
      ],
      [text("-")],
    );
  ignore(del_abbr_btn_view);

  // type view_info = (Exercise.pos, VerifiedTree.res, ed)
  // and ed =
  //   | Just(Derivation.Rule.t, Editor.t, Exercise.DynamicsItem.t)
  //   | Abbr(index);

  let derivation_wrapper = l =>
    switch (List.rev(l)) {
    | [] => failwith("derivation_wrapper: empty list")
    | [hd, ...tl] =>
      (tl |> List.rev |> List.mapi(abbreviation_wrapper)) @ [hd]
    };

  let info_tree =
    List.map2(Tree.combine, eds.trees, stitched_dynamics.trees)
    |> List.map(
         Tree.map(
           fun
           | (Exercise.Proof.Just({jdmt, rule}), Some(di)) =>
             Just(rule, jdmt, di)
           | (Abbr(i), _) => Abbr(i)
           | _ => raise(Failure("DerivationTree.mk: ed<>di inconsistent")),
         ),
       )
    |> List.map2(Tree.combine, grading_report.proof_report.verified_tree)
    |> List.mapi(i =>
         Tree.mapi((pos, (res, ed)) => (Trees(i, pos), res, ed))
       );

  let derivation_view =
    Cell.simple_cell_view([
      Cell.simple_cell_item([
        Cell.caption("Derivation"),
        div(
          ~attrs=[Attr.class_("cell-derivation")],
          info_tree
          |> List.map(Tree.fold_deep(deduction_view))
          |> List.map(t =>
               div(~attrs=[Attr.class_("deduction-tree")], [t])
             )
          |> derivation_wrapper,
        ),
      ]),
    ]);

  let prelude_view =
    editor_view(
      Proof(Prelude),
      ~editor=eds.prelude,
      ~di=stitched_dynamics.prelude,
      ~caption=Cell.caption("Prelude"),
    );

  [prelude_view, derivation_view];
};
