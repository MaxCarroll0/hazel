module Js = Js_of_ocaml.Js;
module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;
module Vdom = Virtual_dom.Vdom;
module Attr = Vdom.Attr;
module Event = Vdom.Event;
module Node = Vdom.Node;

module MeasuredPosition = Pretty.MeasuredPosition;
module MeasuredLayout = Pretty.MeasuredLayout;

let decoration_cls: UHDecorationShape.t => string =
  fun
  | ErrHole => "err-hole"
  | VarErrHole => "var-err-hole"
  | VarUse => "var-use"
  | CurrentTerm => "current-term";

let decoration_view =
    (
      ~contains_current_term: bool,
      ~corner_radii: (float, float),
      ~term_sort: TermSort.t,
      ~term_shape: TermShape.t,
      shape: UHDecorationShape.t,
    ) =>
  UHDecoration.(
    switch (shape) {
    | ErrHole => ErrHole.view(~contains_current_term, ~corner_radii)
    | VarErrHole => VarErrHole.view(~contains_current_term, ~corner_radii)
    | VarUse => VarUse.view(~corner_radii)
    | CurrentTerm =>
      CurrentTerm.view(~corner_radii, ~sort=term_sort, ~shape=term_shape)
    }
  );

let decoration_views =
    (~font_metrics: FontMetrics.t, dpaths: UHDecorationPaths.t, l: UHLayout.t)
    : list(Vdom.Node.t) => {
  let corner_radii = Decoration_common.corner_radii(font_metrics);

  let rec go =
          (
            ~tl: list(Vdom.Node.t)=[], // tail-recursive
            ~indent=0, // indentation level of `m`
            ~start=MeasuredPosition.zero, // start position of `m`
            dpaths: UHDecorationPaths.t, // paths to decorations within `m`
            m: UHMeasuredLayout.t,
          )
          : list(Vdom.Node.t) => {
    let go' = go(~indent, ~start);
    switch (m.layout) {
    | Linebreak
    | Text(_) => tl
    | Cat(m1, m2) =>
      let mid_row = start.row + MeasuredLayout.height(m1) - 1;
      let mid_col = {
        let (leading, MeasuredLayout.{width: last_width, _}) =
          ListUtil.split_last(m1.metrics);
        let offset =
          switch (leading) {
          | [] => start.col
          | [_, ..._] => indent
          };
        offset + last_width;
      };
      let mid_tl =
        go(~tl, ~indent, ~start={row: mid_row, col: mid_col}, dpaths, m2);
      go'(~tl=mid_tl, dpaths, m1);
    | Align(m) => go(~tl, ~indent=start.col, ~start, dpaths, m)
    | Annot(annot, m) =>
      switch (annot) {
      | Step(step) =>
        let stepped = UHDecorationPaths.take_step(step, dpaths);
        UHDecorationPaths.is_empty(stepped) ? tl : go'(~tl, stepped, m);
      | Term({shape, sort, _}) =>
        let offset = start.col - indent;
        let current_vs =
          UHDecorationPaths.current(shape, dpaths)
          |> List.map((dshape: UHDecorationShape.t) => {
               let cls = decoration_cls(dshape);
               let view =
                 decoration_view(
                   ~contains_current_term=Option.is_some(dpaths.current_term),
                   ~corner_radii,
                   ~term_shape=shape,
                   ~term_sort=sort,
                   dshape,
                   (offset, m),
                 );
               Decoration_common.container(
                 ~font_metrics,
                 ~height=MeasuredLayout.height(m),
                 ~width=MeasuredLayout.width(~offset, m),
                 ~origin=MeasuredPosition.{row: start.row, col: indent},
                 ~cls,
                 [view],
               );
             });
        go'(~tl=current_vs @ tl, dpaths, m);
      | _ => go'(~tl, dpaths, m)
      }
    };
  };

  go(dpaths, UHMeasuredLayout.mk(l));
};

let click_to_move =
    (root_id: string, font_metrics: FontMetrics.t, evt): ModelAction.t => {
  let container_rect =
    JSUtil.force_get_elem_by_id(root_id)##getBoundingClientRect;
  let (target_x, target_y) = (
    float_of_int(evt##.clientX),
    float_of_int(evt##.clientY),
  );
  let caret_pos =
    Pretty.MeasuredPosition.{
      row:
        Float.to_int(
          (target_y -. container_rect##.top) /. font_metrics.row_height,
        ),
      col:
        Float.to_int(
          Float.round(
            (target_x -. container_rect##.left) /. font_metrics.col_width,
          ),
        ),
    };
  Chain([
    MoveAction(Click(caret_pos)),
    UpdateAssistant(Reset) // reset assistant selection
  ]);
};

let key_handlers =
    (~inject: ModelAction.t => Ui_event.t, ~model: Model.t): list(Attr.t) => {
  [
    Attr.on_keypress(_ => Event.Prevent_default),
    Attr.on_keydown(evt => {
      switch (HazelKeyCombos.of_evt(evt)) {
      | Some(kc) =>
        Event.Many([
          Event.Prevent_default,
          Event.Stop_propagation,
          inject(KeyComboAction.get_model_action(model, kc)),
        ])
      | None => Event.Ignore
      }
    }),
  ];
};

let box_table: WeakMap.t(UHBox.t, list(Vdom.Node.t)) = WeakMap.mk();
let rec view_of_box = (box: UHBox.t): list(Vdom.Node.t) => {
  switch (WeakMap.get(box_table, box)) {
  | Some(vs) => vs
  | None =>
    switch (box) {
    | Text(s) => StringUtil.is_empty(s) ? [] : [Node.text(s)]
    | HBox(boxes) => boxes |> List.map(view_of_box) |> List.flatten
    | VBox(boxes) =>
      let vs =
        boxes
        |> List.map(view_of_box)
        |> ListUtil.join([Node.br([])])
        |> List.flatten;
      [Node.div([Attr.classes(["VBox"])], vs)];
    | Annot(annot, box) =>
      let vs = view_of_box(box);
      switch (annot) {
      | Token({shape, _}) =>
        let clss =
          switch (shape) {
          | Text => ["code-text"]
          | Op => ["code-op"]
          | Delim(_) => ["code-delim"]
          };
        [Node.span([Attr.classes(clss)], vs)];
      | HoleLabel({len}) =>
        let width = Css_gen.width(`Ch(float_of_int(len)));
        [
          Node.span(
            [Attr.style(width), Attr.classes(["HoleLabel"])],
            [Node.span([Attr.classes(["HoleNumber"])], vs)],
          ),
        ];
      | UserNewline => [Node.span([Attr.classes(["UserNewline"])], vs)]
      | CommentLine => [Node.span([Attr.classes(["CommentLine"])], vs)]
      | _ => vs
      };
    }
  };
};

let focus = (editor: Model.editor) =>
  try(JSUtil.force_get_elem_by_id(Model.editor_id(editor))##focus) {
  | _ =>
    Printf.printf(
      "WARNING: tried to focus editor not present in the DOM: %s",
      Model.editor_id(editor),
    )
  };

let exp_view =
    (
      ~settings: Settings.t,
      ~font_metrics: FontMetrics.t,
      ~is_focused: bool,
      program: Editor.exp,
    )
    : list(Node.t) => {
  let layout = Editor.Exp.get_layout(~settings, program);
  let code_text = layout |> UHBox.mk |> view_of_box;
  let dpaths = Editor.Exp.get_decoration_paths(program, is_focused);
  let decorations = decoration_views(~font_metrics, dpaths, layout);
  let caret_pos = Editor.Exp.get_caret_position(~settings, program);
  let caret =
    is_focused ? [UHDecoration.Caret.view(~font_metrics, caret_pos)] : [];
  caret @ [Node.span([Attr.classes(["code"])], code_text)] @ decorations;
};

let pat_view =
    (
      ~settings: Settings.t,
      ~font_metrics: FontMetrics.t,
      ~is_focused: bool,
      program: Editor.pat,
    )
    : list(Node.t) => {
  let layout = Editor.Pat.get_layout(~settings, program);
  let code_text = layout |> UHBox.mk |> view_of_box;
  let dpaths = Editor.Pat.get_decoration_paths(program, is_focused);
  let decorations = decoration_views(~font_metrics, dpaths, layout);
  let caret_pos = Editor.Pat.get_caret_position(~settings, program);
  let caret =
    is_focused ? [UHDecoration.Caret.view(~font_metrics, caret_pos)] : [];
  caret @ [Node.span([Attr.classes(["code"])], code_text)] @ decorations;
};

let typ_view =
    (
      ~settings: Settings.t,
      ~font_metrics: FontMetrics.t,
      ~is_focused: bool,
      editor: Editor.typ,
    ) => {
  let layout = Editor.Typ.get_layout(~settings, editor);
  let code_text = layout |> UHBox.mk |> view_of_box;
  let dpaths = Editor.Typ.get_decoration_paths(editor, is_focused);
  let decorations = decoration_views(~font_metrics, dpaths, layout);
  let caret_pos = Editor.Typ.get_caret_position(~settings, editor);
  let caret =
    is_focused ? [UHDecoration.Caret.view(~font_metrics, caret_pos)] : [];
  caret @ [Node.span([Attr.classes(["code"])], code_text)] @ decorations;
};

let view_syntax =
    (
      ~settings: Settings.t,
      ~font_metrics: FontMetrics.t,
      ~is_focused: bool,
      syntax: TermSort.syntax,
    )
    : list(Node.t) =>
  switch (syntax) {
  | Exp(uhexp) =>
    exp_view(
      ~is_focused,
      ~settings,
      ~font_metrics,
      Editor.mk_exp_editor(uhexp),
    )
  | Pat(uhpat) =>
    pat_view(
      ~is_focused,
      ~settings,
      ~font_metrics,
      Editor.mk_pat_editor(uhpat),
    )
  | Typ(uhtyp) =>
    typ_view(
      ~is_focused,
      ~settings,
      ~font_metrics,
      Editor.mk_typ_editor(uhtyp),
    )
  };

let filter_typ_view =
    (
      ~inject: ModelAction.t => Ui_event.t,
      ~font_metrics: FontMetrics.t,
      ~settings: Settings.t,
      ~model: Model.t,
      ~is_focused: bool,
      editor: Editor.typ,
      _u_gen,
    ) => {
  let this_editor = Model.AssistantTypeEditor;
  let editor_id = Model.editor_id(this_editor);
  let key_handlers = is_focused ? key_handlers(~inject, ~model) : [];
  let move = evt => inject(click_to_move(editor_id, font_metrics, evt));
  [
    Node.div(
      [
        Attr.id(editor_id),
        Attr.classes(["code"]),
        Attr.on_click(move),
        Attr.create("tabindex", "0"), // necessary to make cell focusable
        Attr.on_focus(_ => inject(FocusCell(this_editor))),
        Attr.on_blur(_ => inject(BlurCell)),
        ...key_handlers,
      ],
      typ_view(~settings, ~font_metrics, ~is_focused, editor),
    ),
  ];
};
