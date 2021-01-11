open Sexplib.Std;

module Dom_html = Js_of_ocaml.Dom_html;
module Dom = Js_of_ocaml.Dom;
module Js = Js_of_ocaml.Js;
open Virtual_dom.Vdom;

module LivelitView = {
  type div_type = Node.t;

  type splice_and_param_getters = {
    uhcode: SpliceName.t => div_type,
    dhcode: SpliceName.t => option((DHExp.t, div_type)),
    dargs: option(list((Var.t, option((DHExp.t, div_type))))),
  };

  type t = splice_and_param_getters => div_type;
};

module type LIVELIT = {
  let name: LivelitName.t;
  let expansion_ty: HTyp.t;
  let param_tys: list((Var.t, HTyp.t));

  [@deriving sexp]
  type model;
  [@deriving sexp]
  type action;
  type trigger = action => Event.t;
  type sync = action => unit;

  let init_model: SpliceGenCmd.t(model);
  let update: (model, action) => SpliceGenCmd.t(model);
  let view: (model, trigger, sync) => LivelitView.t;
  let view_shape: model => LivelitShape.t;
  let expand: model => UHExp.t;
};

module LivelitAdapter = (L: LIVELIT) => {
  let serialize_monad = model => SpliceGenCmd.return(L.sexp_of_model(model));

  /* generate livelit definition for Semantics */
  let livelit_defn =
    LivelitDefinition.{
      name: L.name,
      expansion_ty: L.expansion_ty,
      param_tys: L.param_tys,
      init_model: SpliceGenCmd.bind(L.init_model, serialize_monad),
      update: (serialized_model, serialized_action) =>
        SpliceGenCmd.bind(
          L.update(
            L.model_of_sexp(serialized_model),
            L.action_of_sexp(serialized_action),
          ),
          serialize_monad,
        ),
      expand: serialized_model =>
        Success(L.expand(L.model_of_sexp(serialized_model))),
    };

  let serialized_view_fn = (serialized_model, trigger, sync) =>
    L.view(
      L.model_of_sexp(serialized_model),
      action => trigger(L.sexp_of_action(action)),
      action => sync(L.sexp_of_action(action)),
    );

  let serialized_view_shape_fn = serialized_model =>
    L.view_shape(L.model_of_sexp(serialized_model));

  let contexts_entry = (
    L.name,
    livelit_defn,
    serialized_view_fn,
    serialized_view_shape_fn,
  );
};

type trigger_serialized = SerializedAction.t => Event.t;
type sync_serialized = SerializedAction.t => unit;
type serialized_view_fn_t =
  (SerializedModel.t, trigger_serialized, sync_serialized) => LivelitView.t;
type serialized_view_shape_fn_t = SerializedModel.t => LivelitShape.t;

module LivelitViewCtx = {
  type t = VarMap.t_((serialized_view_fn_t, serialized_view_shape_fn_t));
  include VarMap;
};

module LivelitContexts = {
  type t('a) = (LivelitCtx.t('a), LivelitViewCtx.t);
  let empty = (LivelitCtx.empty, LivelitViewCtx.empty);
  let extend =
      (
        (livelit_ctx, livelit_view_ctx),
        (
          name,
          def: LivelitDefinition.t,
          serialized_view_fn,
          serialized_view_shape_fn,
        ),
      ) => {
    if (!LivelitName.is_valid(name)) {
      failwith("Invalid livelit name " ++ name);
    };
    if (name != def.name) {
      failwith(
        "Livelit name " ++ name ++ " differs from def name " ++ def.name,
      );
    };
    let (param_names, _) = List.split(def.param_tys);
    let rec contains_dupl =
      fun
      | [] => false
      | [hd, ...tl] => List.mem(hd, tl) || contains_dupl(tl);
    if (contains_dupl(param_names)) {
      failwith(
        "Parameter names for livelit "
        ++ name
        ++ " must be unique: "
        ++ String.concat(", ", param_names),
      );
    };
    (
      VarMap.extend(livelit_ctx, (name, (def, []))),
      VarMap.extend(
        livelit_view_ctx,
        (name, (serialized_view_fn, serialized_view_shape_fn)),
      ),
    );
  };
};

let _to_uhvar = id => UHExp.var(SpliceInfo.var_of_splice_name(id));

let attr_style = Attr.create("style");

let prop_val = (prop: string, value: string) =>
  StringUtil.cat([prop, ": ", value, ";"]);

module PairLivelit: LIVELIT = {
  let name = "$pair";
  let expansion_ty = HTyp.(Prod([Hole, Hole]));
  let param_tys = [];

  [@deriving sexp]
  type model = (int, int);
  [@deriving sexp]
  type action = unit;
  type trigger = action => Event.t;
  type sync = action => unit;

  let init_model =
    SpliceGenCmd.bind(SpliceGenCmd.new_splice(HTyp.Hole), leftID =>
      SpliceGenCmd.bind(SpliceGenCmd.new_splice(HTyp.Hole), rightID =>
        SpliceGenCmd.return((leftID, rightID))
      )
    );
  let update = (m, _) => SpliceGenCmd.return(m);

  let view =
      (
        (leftID, rightID),
        _,
        _,
        {uhcode, _}: LivelitView.splice_and_param_getters,
      ) =>
    Node.div(
      [Attr.classes(["pair-livelit"])],
      [uhcode(leftID), uhcode(rightID)],
    );
  let view_shape = _ =>
    LivelitShape.Inline(
      // TODO fix brittle magic constant
      20,
    );

  let expand = ((leftID, rightID)) => {
    let pair_seq =
      Seq.mk(
        _to_uhvar(leftID),
        [(Operators_Exp.Comma, _to_uhvar(rightID))],
      );
    UHExp.Block.wrap'(UHExp.mk_OpSeq(pair_seq));
  };
};

module type MAT_INFO = {
  let name: LivelitName.t;
  let is_live: bool;
};

module MatrixLivelitFunctor = (I: MAT_INFO) : LIVELIT => {
  let name = I.name;
  let expansion_ty = HTyp.(List(List(Int)));
  let param_tys = [];

  // assume nonzero height and width
  [@deriving sexp]
  type model = (SpliceName.t, list(list(SpliceName.t)));
  [@deriving sexp]
  type dim =
    | Row
    | Col;
  [@deriving sexp]
  type action =
    | Select(SpliceName.t)
    | Add(dim)
    | Del(dim, int);
  type trigger = action => Event.t;
  type sync = action => unit;

  let init_height = 2;
  let init_width = 2;

  let get_height = (m: list(list(SpliceName.t))): int => List.length(m);
  let get_width = (m: list(list(SpliceName.t))): int =>
    List.length(List.hd(m));

  let init_model =
    SpliceGenCmd.(
      MonadsUtil.bind_count(
        init_height,
        bind(
          MonadsUtil.bind_count(
            init_width,
            bind(new_splice(HTyp.Int)),
            return,
          ),
        ),
        grid =>
        return((grid |> List.hd |> List.hd, grid))
      )
    );

  let update = ((selected, m)) =>
    fun
    | Select(to_select) => {
        let to_select_in_grid =
          !(m |> List.for_all(List.for_all(s => s != to_select)));
        if (!to_select_in_grid) {
          JSUtil.log(
            Printf.sprintf(
              "Attempt to select splice name %d, which is not in the matrix",
              to_select,
            ),
          );
        };
        SpliceGenCmd.return((to_select_in_grid ? to_select : selected, m));
      }
    | Add(Row) =>
      SpliceGenCmd.(
        MonadsUtil.bind_count(
          get_width(m), bind(new_splice(HTyp.Int)), new_row =>
          return((selected, m @ [new_row]))
        )
      )
    | Add(Col) =>
      SpliceGenCmd.(
        MonadsUtil.bind_count(
          get_height(m), bind(new_splice(HTyp.Int)), new_col =>
          return((selected, List.map2((c, r) => r @ [c], new_col, m)))
        )
      )
    | Del(dim, i) => {
        let drop = (to_drop, ret) => {
          let selected_in_to_drop =
            !(to_drop |> List.for_all(s => s != selected));
          let to_select =
            selected_in_to_drop ? ret |> List.hd |> List.hd : selected;
          SpliceGenCmd.(
            MonadsUtil.bind_list(
              to_drop,
              d => bind(drop_splice(d)),
              _ => return((to_select, ret)),
            )
          );
        };
        switch (dim) {
        | Row =>
          if (get_height(m) <= 1) {
            SpliceGenCmd.return((selected, m));
          } else {
            let (before, to_drop, after) = ListUtil.split_nth(i, m);
            drop(to_drop, before @ after);
          }
        | Col =>
          if (get_width(m) <= 1) {
            SpliceGenCmd.return((selected, m));
          } else {
            let (before, to_drop, after) =
              m |> List.map(r => ListUtil.split_nth(i, r)) |> ListUtil.split3;
            drop(to_drop, List.map2((b, a) => b @ a, before, after));
          }
        };
      };

  let grid_area = (row_start, col_start, row_end, col_end) =>
    prop_val(
      "grid-area",
      Printf.sprintf(
        "%d / %d / %d / %d",
        row_start,
        col_start,
        row_end,
        col_end,
      ),
    );

  let view =
      (
        (selected, m),
        trig,
        _,
        {uhcode, dhcode, _}: LivelitView.splice_and_param_getters,
      ) => {
    let width = get_width(m);
    let height = get_height(m);
    let row_header =
      ListUtil.range(height)
      |> List.map(i =>
           Node.div(
             [
               attr_style(grid_area(i + 3, 1, i + 4, 2)),
               Attr.classes(["row-header", "pure-button"]),
               Attr.on_click(_ => trig(Del(Row, i))),
             ],
             [
               Node.span(
                 [Attr.classes(["index"])],
                 [Node.text(string_of_int(i + 1))],
               ),
               Node.span([Attr.classes(["delete"])], [Node.text("x")]),
             ],
           )
         );
    let col_header =
      ListUtil.range(width)
      |> List.map(j =>
           Node.div(
             [
               attr_style(grid_area(1, j + 3, 2, j + 4)),
               Attr.classes(["col-header", "pure-button"]),
               Attr.on_click(_ => trig(Del(Col, j))),
             ],
             [
               Node.span(
                 [Attr.classes(["index"])],
                 [Node.text(string_of_int(j + 1))],
               ),
               Node.span([Attr.classes(["delete"])], [Node.text("x")]),
             ],
           )
         );
    let add_row_button =
      Node.button(
        [
          attr_style(grid_area(-1, 3, -2, -3)),
          Attr.classes(["add-row", "pure-button"]),
          Attr.on_click(_ => trig(Add(Row))),
        ],
        [Node.text("+")],
      );
    let add_col_button =
      Node.button(
        [
          attr_style(grid_area(3, -2, -3, -1)),
          Attr.classes(["add-col", "pure-button"]),
          Attr.on_click(_ => trig(Add(Col))),
        ],
        [Node.text("+")],
      );
    let maybe_add_formula_bar = rest =>
      if (!I.is_live) {
        rest;
      } else {
        Node.div(
          [Attr.classes(["matrix-livelit"])],
          [
            Node.div(
              [Attr.classes(["matrix-formula-bar"])],
              [
                Node.span(
                  [Attr.classes(["matrix-formula-bar-text"])],
                  [Node.text("selected cell's formula: ")],
                ),
                Node.div(
                  [Attr.classes(["matrix-formula-bar-splice"])],
                  [uhcode(selected)],
                ),
              ],
            ),
            rest,
          ],
        );
      };
    let cells =
      m
      |> List.mapi((i, row) =>
           row
           |> List.mapi((j, splice) => {
                let contents =
                  if (!I.is_live) {
                    Node.div(
                      [Attr.classes(["matrix-splice"])],
                      [uhcode(splice)],
                    );
                  } else {
                    let cls =
                      splice == selected
                        ? "matrix-selected" : "matrix-unselected";
                    let child =
                      switch (dhcode(splice)) {
                      | None => Node.text("Uneval'd")
                      | Some((_, view)) => view
                      };
                    Node.div(
                      [
                        Attr.classes([cls]),
                        Attr.on_mousedown(_ => trig(Select(splice))),
                      ],
                      [child],
                    );
                  };
                Node.div(
                  [
                    attr_style(grid_area(i + 3, j + 3, i + 4, j + 4)),
                    Attr.classes(["matrix-cell"]),
                  ],
                  [contents],
                );
              })
         )
      |> List.flatten;

    let cells_border =
      Node.div(
        [
          attr_style(grid_area(3, 3, -3, -3)),
          Attr.classes(["cells-border"]),
        ],
        [],
      );

    let gutter_width = "10px";
    let dim_template = dim =>
      StringUtil.sep(
        List.concat([
          ["auto", gutter_width],
          ListUtil.replicate(dim, "auto"),
          [gutter_width, "auto"],
        ]),
      );
    maybe_add_formula_bar(
      Node.div(
        [
          Attr.classes(["matrix-livelit"]),
          attr_style(
            StringUtil.cat([
              prop_val("grid-template-columns", dim_template(width)),
              prop_val("grid-template-rows", dim_template(height)),
            ]),
          ),
        ],
        List.concat([
          row_header,
          col_header,
          [cells_border, ...cells],
          [add_row_button, add_col_button],
        ]),
      ),
    );
  };

  let view_shape = ((_, matrix)) => {
    let num_rows = List.length(matrix);
    LivelitShape.MultiLine(3 * num_rows + 2 + (I.is_live ? 1 : 0));
  };

  let expand = ((_, m)) => {
    let to_uhexp_list =
      fun
      | [] => UHExp.(Block.wrap(ListNil(NotInHole)))
      | [fst, ...rest] => {
          let rest' =
            (rest |> List.map(item => (Operators_Exp.Cons, item)))
            @ [(Operators_Exp.Cons, UHExp.ListNil(NotInHole))];
          let seq = Seq.mk(fst, rest');
          UHExp.Block.wrap'(UHExp.mk_OpSeq(seq));
        };
    let m' =
      m
      |> List.map(r =>
           r
           |> List.map(_to_uhvar)
           |> to_uhexp_list
           |> (q => UHExp.Parenthesized(q))
         );
    to_uhexp_list(m');
  };
};

module MatrixLivelitInfo: MAT_INFO = {
  let name = "$matrix";
  let is_live = false;
};
module MatrixLivelit = MatrixLivelitFunctor(MatrixLivelitInfo);
module LiveMatrixLivelitInfo: MAT_INFO = {
  let name = "$live_matrix";
  let is_live = true;
};
module LiveMatrixLivelit = MatrixLivelitFunctor(LiveMatrixLivelitInfo);

module GradeCutoffLivelit: LIVELIT = {
  let name = "$grade_cutoffs";
  let expansion_ty = HTyp.(Prod([Int, Int, Int, Int]));
  let param_tys = [("data", HTyp.(List(Int)))];

  [@deriving sexp]
  type letter_grade =
    | A
    | B
    | C
    | D;

  [@deriving sexp]
  type model = {
    a: int,
    b: int,
    c: int,
    d: int,
    selecting: option(letter_grade),
  };

  [@deriving sexp]
  type action =
    | UpdateCutoff(letter_grade, int)
    | StartSelecting(letter_grade)
    | StopSelecting;

  type trigger = action => Event.t;
  type sync = action => unit;

  let init_model =
    SpliceGenCmd.return({a: 90, b: 80, c: 70, d: 60, selecting: None});

  let is_valid_percentage = (p: int) => 0 <= p && p <= 100;

  let rec update_a = (new_a, model): model =>
    if (!is_valid_percentage(new_a)) {
      model;
    } else {
      let updated = {...model, a: new_a};
      if (updated.a < updated.b) {
        update_b(updated.a - 1, updated);
      } else {
        updated;
      };
    }
  and update_b = (new_b, model): model =>
    if (!is_valid_percentage(new_b)) {
      model;
    } else {
      let updated = {...model, b: new_b};
      if (updated.a <= updated.b) {
        update_a(updated.b + 1, updated);
      } else if (updated.b <= updated.c) {
        update_c(updated.b - 1, updated);
      } else {
        updated;
      };
    }
  and update_c = (new_c, model): model =>
    if (!is_valid_percentage(new_c)) {
      model;
    } else {
      let updated = {...model, c: new_c};
      if (updated.b <= updated.c) {
        update_b(updated.c + 1, updated);
      } else if (updated.c <= updated.d) {
        update_d(updated.c - 1, updated);
      } else {
        updated;
      };
    }
  and update_d = (new_d, model): model =>
    if (!is_valid_percentage(new_d)) {
      model;
    } else {
      let updated = {...model, d: new_d};
      if (updated.c <= updated.d) {
        update_c(updated.c + 1, updated);
      } else {
        updated;
      };
    };

  let update = (model, action) =>
    SpliceGenCmd.return(
      switch (action) {
      | StopSelecting => {...model, selecting: None}
      | StartSelecting(letter) => {...model, selecting: Some(letter)}
      | UpdateCutoff(letter, new_cutoff) =>
        let update =
          switch (letter) {
          | A => update_a(new_cutoff)
          | B => update_b(new_cutoff)
          | C => update_c(new_cutoff)
          | D => update_d(new_cutoff)
          };
        update(model);
      },
    );

  let grades_invalids_to_svgs = ((grades, invalid_count)) => {
    let valid_grades =
      grades
      |> List.filter_map(grade =>
           if (0 <= grade && grade <= 100) {
             Some(
               Node.create_svg(
                 "circle",
                 [
                   Attr.create("cx", string_of_int(grade)),
                   Attr.create("cy", "0"),
                   Attr.create("r", "0.75"),
                   Attr.create("fill", "orange"),
                   Attr.create("stroke-width", "0"),
                 ],
                 [],
               ),
             );
           } else {
             None;
           }
         );
    (
      valid_grades,
      invalid_count + List.length(grades) - List.length(valid_grades),
    );
  };

  let rec dhexp_to_grades_invalids = (rslt, invalid_count) =>
    fun
    | DHExp.Cons(DHExp.IntLit(g), d) =>
      dhexp_to_grades_invalids([g, ...rslt], invalid_count, d)
    | DHExp.Cons(_, d) =>
      dhexp_to_grades_invalids(rslt, invalid_count + 1, d)
    | DHExp.ListNil(_) => (List.rev(rslt), invalid_count)
    | _ => (List.rev(rslt), invalid_count + 1);

  let view =
      (
        {a, b, c, d, selecting},
        trigger,
        _,
        {dargs: _, _}: LivelitView.splice_and_param_getters,
      ) => {
    /*
     let data_opt =
       switch (dargs) {
       | None
       | Some([(_, None)]) => None
       | Some([(_, Some((d, _)))]) => Some(d)
       | Some(l) =>
         failwith(
           "Invalid grade_cutoffs params: "
           ++ (l |> List.map(((s, _)) => s) |> String.concat(", ")),
         )
       };
     */
    let grades = [93, 88, 75, 86, 78, 82, 67, 54, 45, 71, 69, 62, 97, 83, 85];
    let data_opt =
      Some(
        grades
        |> List.fold_left(
             (acc, g) => DHExp.Cons(IntLit(g), acc),
             DHExp.ListNil(Int),
           ),
      );
    let grades_svgs_invalids_opt =
      data_opt
      |> Option.map(d =>
           dhexp_to_grades_invalids([], 0, d) |> grades_invalids_to_svgs
         );
    let (grades_svgs, data_err_msg) =
      switch (grades_svgs_invalids_opt) {
      | None => ([], [Node.text("Grades data was never evaluated")])
      | Some((grades_svgs, invalid_count)) => (
          grades_svgs,
          if (invalid_count == 0) {
            [];
          } else if (invalid_count == 1) {
            [Node.text("1 grade was indeterminate or out of bounds")];
          } else {
            [
              Node.text(
                Printf.sprintf(
                  "%d grades were indeterminate or out of bounds",
                  invalid_count,
                ),
              ),
            ];
          },
        )
      };

    let px_scalar = 5.;
    let scale_len = 100.;
    let thumb_radius = 3.;
    let thumb_tip = 5.;

    let cutoff_thumb = (letter: letter_grade, value: int) => {
      open SvgUtil;
      let v = Float.of_int(value);
      let chord_distance = thumb_radius /. Float.sqrt(2.);
      let arc_start = Point.{x: v -. chord_distance, y: -. thumb_tip};
      let arc_end = Point.{x: v +. chord_distance, y: -. thumb_tip};
      let control = Point.{x: v, y: -. (thumb_tip -. chord_distance)};
      let origin = Point.{x: v, y: (-0.5)};
      let thumb =
        Path.[
          Q({control, target: arc_start}),
          A({
            rx: thumb_radius,
            ry: thumb_radius,
            x_axis_rotation: 0.,
            large_arc_flag: true,
            sweep_flag: true,
            target: arc_end,
          }),
          Q({control, target: origin}),
        ];
      Node.create_svg(
        "g",
        [],
        [
          Path.view(
            ~attrs=
              Attr.[
                classes(["grade-cutoff-thumb"]),
                create("vector-effect", "non-scaling-stroke"),
                on_mousedown(_ => trigger(StartSelecting(letter))),
              ],
            [M(origin), ...thumb],
          ),
          Node.create_svg(
            "text",
            Attr.[
              classes(["grade-label"]),
              create("vector-effect", "non-scaling-stroke"),
              create("dominant-baseline", "middle"),
              create("text-anchor", "middle"),
              create("x", Printf.sprintf("%f", v)),
              create(
                "y",
                Printf.sprintf("%f", -. (thumb_tip +. chord_distance -. 0.4)),
              ),
              on_mousedown(_ => trigger(StartSelecting(letter))),
            ],
            [
              Node.text(
                switch (letter) {
                | A => "A"
                | B => "B"
                | C => "C"
                | D => "D"
                },
              ),
            ],
          ),
        ],
      );
    };

    let percentage_line = grade_points => {
      let percentage_label = (p: int) =>
        Node.create_svg(
          "text",
          [
            Attr.classes(["grade-cutoff-scale-label"]),
            Attr.create("x", string_of_int(p)),
            Attr.create("y", "1"),
            Attr.create("dominant-baseline", "hanging"),
            Attr.create("text-anchor", "middle"),
            Attr.create("vector-effect", "non-scaling-stroke"),
          ],
          [Node.text(string_of_int(p))],
        );
      let percentage_labels =
        ListUtil.range(~lo=0, 11)
        |> List.map(( * )(10))
        |> List.map(percentage_label);
      Node.create_svg(
        "g",
        [],
        [
          Node.create_svg(
            "line",
            [
              Attr.classes(["grade-cutoff-scale"]),
              Attr.create("x1", "0"),
              Attr.create("y1", "0"),
              Attr.create("x2", Printf.sprintf("%f", scale_len)),
              Attr.create("y2", "0"),
              Attr.create("vector-effect", "non-scaling-stroke"),
            ],
            [],
          ),
          ...percentage_labels,
        ]
        @ grade_points,
      );
    };

    let distribution_line = {
      let (fs, ds) = List.partition(g => g < d, grades);
      let (ds, cs) = List.partition(g => g < c, ds);
      let (cs, bs) = List.partition(g => g < b, cs);
      let (bs, as_) = List.partition(g => g < a, bs);
      let line_y = (-12.);
      // let line =
      //   Node.create_svg(
      //     "line",
      //     [
      //       Attr.classes(["distribution-line"]),
      //       Attr.create("x1", "0"),
      //       Attr.create("y1", string_of_float(line_y)),
      //       Attr.create("x2", Printf.sprintf("%f", scale_len)),
      //       Attr.create("y2", string_of_float(line_y)),
      //       Attr.create("vector-effect", "non-scaling-stroke"),
      //     ],
      //     [],
      //   );
      let labeled_bucket = (x1, x2, num) => {
        let len = (-0.75);
        let bucket =
          SvgUtil.Path.[
            M({x: x1, y: line_y -. len}),
            V({y: line_y}),
            H({x: x2}),
            V({y: line_y -. len}),
          ]
          |> SvgUtil.Path.view(
               ~attrs=[
                 Attr.classes(["bucket-path"]),
                 Attr.create("vector-effect", "non-scaling-stroke"),
               ],
             );
        let label =
          Node.create_svg(
            "text",
            [
              Attr.classes(["grade-cutoff-bucket-label"]),
              Attr.create("x", string_of_float((x1 +. x2) *. 0.5)),
              Attr.create("y", string_of_float(line_y -. 1.)),
              Attr.create("dominant-baseline", "auto"),
              Attr.create("text-anchor", "middle"),
              Attr.create("vector-effect", "non-scaling-stroke"),
            ],
            [Node.text(string_of_int(num))],
          );
        [bucket, label];
      };
      let buffer = 0.4;
      let fs_bucket =
        labeled_bucket(0., Float.of_int(d) -. buffer, List.length(fs));
      let ds_bucket =
        labeled_bucket(
          Float.of_int(d) +. buffer,
          Float.of_int(c) -. buffer,
          List.length(ds),
        );
      let cs_bucket =
        labeled_bucket(
          Float.of_int(c) +. buffer,
          Float.of_int(b) -. buffer,
          List.length(cs),
        );
      let bs_bucket =
        labeled_bucket(
          Float.of_int(b) +. buffer,
          Float.of_int(a) -. buffer,
          List.length(bs),
        );
      let as_bucket =
        labeled_bucket(Float.of_int(a) +. buffer, 100., List.length(as_));
      fs_bucket @ ds_bucket @ cs_bucket @ bs_bucket @ as_bucket;
    };

    let thumbs = {
      let (a, b, c, d) = (
        cutoff_thumb(A, a),
        cutoff_thumb(B, b),
        cutoff_thumb(C, c),
        cutoff_thumb(D, d),
      );
      // change paint order depending on
      // which thumb is being dragged
      switch (selecting) {
      | None
      | Some(A) => [d, c, b, a]
      | Some(B) => [d, c, a, b]
      | Some(C) => [a, d, b, c]
      | Some(D) => [a, b, c, d]
      };
    };

    let overlay =
      switch (selecting) {
      | None => []
      | Some(letter) => [
          Node.div(
            Attr.[
              classes(["dragging-overlay"]),
              on_mouseup(_ => trigger(StopSelecting)),
              on_mousemove(evt => {
                let scale =
                  JSUtil.force_get_elem_by_cls("grade-cutoff-scale")##getBoundingClientRect;
                let offset_x = Float.of_int(Js.Unsafe.get(evt, "offsetX"));
                let cutoff =
                  100.
                  *. (offset_x -. scale##.left)
                  /. (px_scalar *. scale_len);
                trigger(UpdateCutoff(letter, Float.to_int(cutoff)));
              }),
            ],
            [],
          ),
        ]
      };

    let width = scale_len +. 2. *. thumb_radius +. 2.;
    let height = 2. *. thumb_radius +. thumb_tip +. 10.;
    Node.div(
      [Attr.classes(["grade-cutoffs-livelit"])],
      [
        Node.create_svg(
          "svg",
          [
            Attr.classes(["grade-display"]),
            Attr.create(
              "viewBox",
              Printf.sprintf(
                "%f %f %f %f",
                -. (thumb_radius +. 1.),
                -. (2. *. thumb_radius +. thumb_tip +. 6.),
                width,
                height,
              ),
            ),
            Attr.create("width", Printf.sprintf("%fpx", px_scalar *. width)),
            Attr.create(
              "height",
              Printf.sprintf("%fpx", px_scalar *. height),
            ),
            Attr.create("stroke", "black"),
          ],
          [percentage_line(grades_svgs), ...thumbs] @ distribution_line,
        ),
        Node.div([Attr.classes(["data-err-msg"])], data_err_msg),
        ...overlay,
      ],
    );
  };

  let view_shape = _ => {
    LivelitShape.MultiLine(
      // TODO
      6,
    );
  };

  let expand = ({a, b, c, d, selecting: _}) => {
    let tupl_seq =
      UHExp.(
        Seq.mk(
          intlit'(a),
          [
            (Operators_Exp.Comma, intlit'(b)),
            (Operators_Exp.Comma, intlit'(c)),
            (Operators_Exp.Comma, intlit'(d)),
          ],
        )
      );
    UHExp.Block.wrap'(UHExp.mk_OpSeq(tupl_seq));
  };
};

module GrayscaleLivelit: LIVELIT = {
  let name = "$basic_adjustments";
  let expansion_ty = HTyp.Int;
  let param_tys = [("url", HTyp.String)];

  [@deriving sexp]
  type model = {
    brightness: SpliceName.t,
    grayscale: SpliceName.t,
  };
  [@deriving sexp]
  type action = unit;
  type trigger = action => Event.t;
  type sync = action => unit;

  let init_model =
    SpliceGenCmd.(
      bind(
        new_splice(
          ~init_uhexp_gen=
            u_gen => (UHExp.(Block.wrap(intlit'(100))), u_gen),
          HTyp.Int,
        ),
        brightness =>
        bind(
          new_splice(
            ~init_uhexp_gen=
              u_gen => (UHExp.(Block.wrap(intlit'(100))), u_gen),
            HTyp.Int,
          ),
          grayscale =>
          return({brightness, grayscale})
        )
      )
    );

  let update = (model, _) => SpliceGenCmd.return(model);

  let height = 17;
  let view_shape = _ => LivelitShape.MultiLine(height);

  let view =
      (
        {brightness, grayscale},
        _,
        _,
        {dargs, dhcode, uhcode}: LivelitView.splice_and_param_getters,
      ) => {
    let subject = {
      switch (dargs, dhcode(brightness), dhcode(grayscale)) {
      | (
          Some([("url", Some((DHExp.StringLit(url), _)))]),
          Some((DHExp.IntLit(b), _)),
          Some((DHExp.IntLit(g), _)),
        )
          when 0 <= b && b <= 100 && 0 <= g && g <= 100 =>
        Node.create(
          "img",
          [
            Attr.classes(["subject"]),
            Attr.create("src", url),
            Attr.create(
              "style",
              Printf.sprintf(
                "filter: brightness(%d%%) grayscale(%d%%);",
                b,
                g,
              ),
            ),
          ],
          [],
        )
      | _ =>
        Node.div(
          [
            Attr.classes(["missing-subject"]),
            Attr.create(
              "style",
              Printf.sprintf("background-color: gray; width: 550px;"),
            ),
          ],
          [],
        )
      };
    };
    Node.div(
      [Attr.classes(["grayscale-livelit"])],
      [
        Node.div(
          [Attr.classes(["slider-bar"])],
          [
            Node.create(
              "img",
              [
                Attr.id("grayscale-icon"),
                Attr.create("src", "imgs/grayscale-icon.png"),
              ],
              [],
            ),
            Node.div(
              [Attr.classes(["splice-content"])],
              [uhcode(grayscale)],
            ),
            Node.div([], []),
            Node.create(
              "img",
              [
                Attr.id("brightness-icon"),
                Attr.create(
                  "src",
                  "https://cdn2.iconfinder.com/data/icons/ecommerce-1-4/65/18-512.png",
                ),
              ],
              [],
            ),
            Node.div(
              [Attr.classes(["splice-content"])],
              [uhcode(brightness)],
            ),
          ],
        ),
        subject,
      ],
    );
  };

  let expand = _ => UHExp.Block.wrap(UHExp.intlit'(0));
};

module ColorLivelit: LIVELIT = {
  let name = "$color";
  let expansion_ty = HTyp.Prod([Int, Int, Int, Int]);
  let param_tys = [];

  // https://github.com/microsoft/vscode/blob/6d4f8310a96ae2dfb7eaa8d1807774fb14b3b263/src/vs/base/common/color.ts#L197
  let hsv_of_rgb = ((r, g, b)) => {
    let r = Float.of_int(r) /. 255.0;
    let g = Float.of_int(g) /. 255.0;
    let b = Float.of_int(b) /. 255.0;
    let cmax = max(r, max(g, b));
    let cmin = min(r, min(g, b));
    let delta = cmax -. cmin;
    let s = cmax == 0.0 ? 0.0 : delta /. cmax;
    let m =
      if (delta == 0.0) {
        0.0;
      } else if (cmax == r) {
        Float.rem(Float.rem((g -. b) /. delta, 6.0) +. 6.0, 6.0);
      } else if (cmax == g) {
        (b -. r) /. delta +. 2.0;
      } else {
        (r -. g) /. delta +. 4.0;
      };

    let h = max(0.0, min(Float.round(m *. 60.0), 360.0));
    let s = max(0.0, min(s, 1.0));
    let v = max(0.0, min(cmax, 1.0));
    (h, s, v);
  };

  // https://github.com/microsoft/vscode/blob/6d4f8310a96ae2dfb7eaa8d1807774fb14b3b263/src/vs/base/common/color.ts#L221
  let rgb_of_hsv = ((h, s, v)) => {
    let c = v *. s;
    let x = c *. (1.0 -. Float.abs(Float.rem(h /. 60.0, 2.0) -. 1.0));
    let m = v -. c;

    let (r, g, b) =
      if (h < 60.0) {
        (c, x, 0.0);
      } else if (h < 120.0) {
        (x, c, 0.0);
      } else if (h < 180.0) {
        (0.0, c, x);
      } else if (h < 240.0) {
        (0.0, x, c);
      } else if (h < 300.0) {
        (x, 0.0, c);
      } else if (h < 360.0) {
        (c, 0.0, x);
      } else {
        (0.0, 0.0, 0.0);
      };
    (
      Float.to_int(Float.round((r +. m) *. 255.0)),
      Float.to_int(Float.round((g +. m) *. 255.0)),
      Float.to_int(Float.round((b +. m) *. 255.0)),
    );
  };

  [@deriving sexp]
  type model = {
    rgb: (SpliceName.t, SpliceName.t, SpliceName.t),
    a: SpliceName.t,
    selecting_sat_val: bool,
  };
  let init_model = {
    let (rval, gval, bval) = (255, 0, 0);
    SpliceGenCmd.(
      bind(
        new_splice(
          ~init_uhexp_gen=
            u_gen => (UHExp.(Block.wrap(intlit'(rval))), u_gen),
          HTyp.Int,
        ),
        r =>
        bind(
          new_splice(
            ~init_uhexp_gen=
              u_gen => (UHExp.(Block.wrap(intlit'(gval))), u_gen),
            HTyp.Int,
          ),
          g =>
          bind(
            new_splice(
              ~init_uhexp_gen=
                u_gen => (UHExp.(Block.wrap(intlit'(bval))), u_gen),
              HTyp.Int,
            ),
            b =>
            bind(
              new_splice(
                ~init_uhexp_gen=
                  u_gen => (UHExp.(Block.wrap(intlit'(100))), u_gen),
                HTyp.Int,
              ),
              a =>
              return({rgb: (r, g, b), a, selecting_sat_val: false})
            )
          )
        )
      )
    );
  };

  [@deriving sexp]
  type action =
    | StartSelectingSatVal
    | StopSelectingSatVal
    | SelectRGB((int, int, int));
  type trigger = action => Event.t;
  type sync = action => unit;

  let update = (m, action) =>
    switch (action) {
    | StartSelectingSatVal =>
      SpliceGenCmd.return({...m, selecting_sat_val: true})
    | StopSelectingSatVal =>
      SpliceGenCmd.return({...m, selecting_sat_val: false})
    | SelectRGB((rval, gval, bval)) =>
      let (r, g, b) = m.rgb;
      SpliceGenCmd.(
        bind(
          map_splice(r, (_, u_gen) =>
            ((HTyp.Int, UHExp.(Block.wrap(intlit'(rval)))), u_gen)
          ),
          _ =>
          bind(
            map_splice(g, (_, u_gen) =>
              ((HTyp.Int, UHExp.(Block.wrap(intlit'(gval)))), u_gen)
            ),
            _ =>
            bind(
              map_splice(b, (_, u_gen) =>
                ((HTyp.Int, UHExp.(Block.wrap(intlit'(bval)))), u_gen)
              ),
              _ =>
              return(m)
            )
          )
        )
      );
    };

  let view =
      (
        {rgb: (r, g, b), a, selecting_sat_val},
        trigger,
        _,
        {uhcode, dhcode, _}: LivelitView.splice_and_param_getters,
      ) => {
    let is_valid = color_value => 0 <= color_value && color_value < 256;
    let is_valid_alpha = a => 0 <= a && a <= 100;
    let rgba_values =
      switch (dhcode(r), dhcode(g), dhcode(b), dhcode(a)) {
      | (
          Some((IntLit(r), _)),
          Some((IntLit(g), _)),
          Some((IntLit(b), _)),
          Some((IntLit(a), _)),
        )
          when
            is_valid(r) && is_valid(g) && is_valid(b) && is_valid_alpha(a) =>
        Some((r, g, b, a))
      | _ => None
      };

    let color_picker = {
      let height = 135.0;
      let width = 135.0;
      let px = Printf.sprintf("%fpx");
      let sat_val_box = {
        let box = (bg_color, attrs, children) => {
          Node.div(
            [
              Attr.classes(["sat-val-box"]),
              attr_style(
                StringUtil.cat([
                  prop_val("height", height |> px),
                  prop_val("width", width |> px),
                  prop_val("background-color", bg_color),
                ]),
              ),
              ...attrs,
            ],
            children,
          );
        };
        switch (rgba_values) {
        | None => box("gray", [], [])
        | Some((rval, gval, bval, _)) =>
          let (h, s, v) = hsv_of_rgb((rval, gval, bval));
          let (sat_r, sat_g, sat_b) = rgb_of_hsv((h, 1.0, 1.0));
          let bounded = f => max(0., min(1., f));
          let offset_x_y = evt =>
            Js.Unsafe.(get(evt, "offsetX"), get(evt, "offsetY"))
            |> TupleUtil.map2(Float.of_int);
          let on_first_click = evt => {
            let (offset_x, offset_y) = offset_x_y(evt);
            let s = offset_x /. width;
            let v = (height -. offset_y) /. height;
            let rgb = rgb_of_hsv((h, bounded(s), bounded(v)));
            Event.Many([
              trigger(StartSelectingSatVal),
              trigger(SelectRGB(rgb)),
            ]);
          };
          let on_drag = evt => {
            let sat_val_rect =
              JSUtil.force_get_elem_by_cls("sat-val-box")##getBoundingClientRect;
            let (offset_x, offset_y) = offset_x_y(evt);
            let s = (offset_x -. sat_val_rect##.left) /. width;
            let v = (height -. (offset_y -. sat_val_rect##.top)) /. height;
            let rgb = rgb_of_hsv((h, bounded(s), bounded(v)));
            trigger(SelectRGB(rgb));
          };
          let overlay =
            Node.div(
              Attr.[
                classes(["dragging-overlay"]),
                on_mouseup(_ => trigger(StopSelectingSatVal)),
                on_mousemove(on_drag),
              ],
              [],
            );
          box(
            Printf.sprintf("rgb(%d, %d, %d)", sat_r, sat_g, sat_b),
            [Attr.on_mousedown(on_first_click)],
            [
              Node.div(
                [
                  Attr.classes(["sat-val-selector"]),
                  attr_style(
                    StringUtil.cat([
                      prop_val("left", s *. width |> px),
                      prop_val("top", height -. v *. height |> px),
                    ]),
                  ),
                  Attr.on_mousedown(_ => trigger(StartSelectingSatVal)),
                ],
                [],
              ),
              ...selecting_sat_val ? [overlay] : [],
            ],
          );
        };
      };

      let hue_slider = {
        let slider = attrs =>
          Node.div(
            [Attr.classes(["hue-slider-wrapper"])],
            [
              Node.input(
                [
                  Attr.classes(["hue-slider"]),
                  Attr.type_("range"),
                  Attr.create("min", "0"),
                  Attr.create("max", "360"),
                  Attr.create(
                    "style",
                    // slider is rotated 90 degrees
                    Printf.sprintf("width: %fpx;", height),
                  ),
                  ...attrs,
                ]
                |> Attrs.merge_classes_and_styles,
                [],
              ),
            ],
          );
        switch (rgba_values) {
        | None => slider([Attr.classes(["no-thumb"])])
        | Some((r, g, b, _)) =>
          let (h, s, v) = hsv_of_rgb((r, g, b));
          slider([
            Attr.value(string_of_int(Float.to_int(h))),
            Attr.on_input((_, value_str) => {
              let h = float_of_string(value_str);
              trigger(SelectRGB(rgb_of_hsv((h, s, v))));
            }),
          ]);
        };
      };

      let color_swatch = {
        let swatch = (attrs, children) =>
          Node.div([Attr.classes(["color-swatch"]), ...attrs], children);
        switch (rgba_values) {
        | None =>
          swatch(
            [],
            [
              Node.create_svg(
                "svg",
                [Attr.create("viewBox", "0 0 100 100")],
                [
                  Node.create_svg(
                    "line",
                    [
                      Attr.create("x1", "20"),
                      Attr.create("y1", "20"),
                      Attr.create("x2", "80"),
                      Attr.create("y2", "80"),
                      Attr.create("vector-effect", "non-scaling-stroke"),
                    ],
                    [],
                  ),
                  Node.create_svg(
                    "line",
                    [
                      Attr.create("x1", "20"),
                      Attr.create("y1", "80"),
                      Attr.create("x2", "80"),
                      Attr.create("y2", "20"),
                      Attr.create("vector-effect", "non-scaling-stroke"),
                    ],
                    [],
                  ),
                ],
              ),
            ],
          )
        | Some((r, g, b, a)) =>
          swatch(
            [
              Attr.create(
                "style",
                Printf.sprintf(
                  "background-color: rgba(%d, %d, %d, %f);",
                  r,
                  g,
                  b,
                  Float.of_int(a) /. 100.0,
                ),
              ),
            ],
            [
              Node.div(
                [Attr.classes(["color-swatch-label"])],
                [
                  Node.span(
                    [
                      // https://stackoverflow.com/questions/3942878/how-to-decide-font-color-in-white-or-black-depending-on-background-color
                      attr_style(
                        prop_val(
                          "color",
                          Float.of_int(r)
                          *. 0.299
                          +. Float.of_int(g)
                          *. 0.587
                          +. Float.of_int(b)
                          *. 0.114 > 186.
                          *. Float.of_int(a)
                          /. 100.
                            ? "#000000" : "#ffffff",
                        ),
                      ),
                    ],
                    [
                      Node.text(
                        Printf.sprintf(
                          "rgba(%d, %d, %d, %.2f)",
                          r,
                          g,
                          b,
                          Float.of_int(a) /. 100.,
                        ),
                      ),
                    ],
                  ),
                ],
              ),
            ],
          )
        };
      };

      let splice_label = lbl =>
        Node.label([Attr.classes(["splice-label"])], [Node.text(lbl)]);
      let splice = splice_name =>
        Node.div(
          [Attr.classes(["splice-content"])],
          [uhcode(splice_name)],
        );
      Node.div(
        [Attr.classes(["color-picker"])],
        [
          color_swatch,
          Node.div(
            [Attr.classes(["rgb-picker"])],
            [
              splice_label("R"),
              splice(r),
              splice_label("G"),
              splice(g),
              splice_label("B"),
              splice(b),
              splice_label("A"),
              splice(a),
            ],
          ),
          Node.div(
            [Attr.classes(["hsv-picker"])],
            [sat_val_box, hue_slider],
          ),
        ],
      );
    };
    Node.div([Attr.classes(["color-livelit"])], [color_picker]);
  };

  let view_shape = _ => LivelitShape.MultiLine(9);

  let expand = ({rgb: (r, g, b), a, _}) => {
    let four_tuple =
      Seq.mk(
        _to_uhvar(r),
        [
          (Operators_Exp.Comma, _to_uhvar(g)),
          (Operators_Exp.Comma, _to_uhvar(b)),
          (Operators_Exp.Comma, _to_uhvar(a)),
        ],
      );
    UHExp.Block.wrap'(UHExp.mk_OpSeq(four_tuple));
  };
};

module ColorLivelitAdapter = LivelitAdapter(ColorLivelit);

module GradientLivelit: LIVELIT = {
  let name = "$gradient";
  let expansion_ty = ColorLivelit.expansion_ty;
  let param_tys = [];

  let (color_livelit_ctx, _) =
    LivelitContexts.extend(
      LivelitContexts.empty,
      ColorLivelitAdapter.contexts_entry,
    );
  let color_ctx = (VarCtx.empty, color_livelit_ctx);

  [@deriving sexp]
  type model = {
    lcolor: SpliceName.t,
    rcolor: SpliceName.t,
    slider_value: int,
  };
  [@deriving sexp]
  type action =
    | Slide(int);
  type trigger = action => Event.t;
  type sync = action => unit;

  let slider_min = 0;
  let slider_max = 100;
  let init_slider_value = 50;
  let init_model = {
    let init_uhexp_gen = u_gen => {
      let (u, u_gen) = MetaVarGen.next_livelit(u_gen);
      let (e, _, u_gen) =
        Statics_Exp.syn_fix_holes(
          color_ctx,
          u_gen,
          UHExp.Block.wrap(FreeLivelit(u, "$color")),
        );
      (e, u_gen);
    };
    SpliceGenCmd.(
      bind(new_splice(~init_uhexp_gen, ColorLivelit.expansion_ty), lcolor =>
        bind(new_splice(~init_uhexp_gen, ColorLivelit.expansion_ty), rcolor =>
          return({lcolor, rcolor, slider_value: init_slider_value})
        )
      )
    );
  };

  let update = (model, a) =>
    switch (a) {
    | Slide(n) => SpliceGenCmd.return({...model, slider_value: n})
    };

  let view =
      (model, trigger, _, {uhcode, _}: LivelitView.splice_and_param_getters) => {
    Node.span(
      [Attr.classes(["gradient-livelit"])],
      [
        uhcode(model.lcolor),
        Node.input(
          [
            Attr.classes(["slider"]),
            Attr.type_("range"),
            Attr.create("min", string_of_int(slider_min)),
            Attr.create("max", string_of_int(slider_max)),
            Attr.value(string_of_int(model.slider_value)),
            Attr.on_change((_, value_str) => {
              let new_value = int_of_string(value_str);
              trigger(Slide(new_value));
            }),
          ],
          [],
        ),
        uhcode(model.rcolor),
      ],
    );
  };

  let view_shape = _ => LivelitShape.Inline(10);

  let expand = ({lcolor, rcolor, slider_value}) => {
    let typ_opseq = (hd, tl) => UHTyp.mk_OpSeq(Seq.mk(hd, tl));
    let pat_opseq = (hd, tl) => UHPat.mk_OpSeq(Seq.mk(hd, tl));
    let exp_opseq = (hd, tl) => UHExp.mk_OpSeq(Seq.mk(hd, tl));
    let typ_triple = (x1, x2, x3) =>
      typ_opseq(x1, [(Operators_Typ.Prod, x2), (Operators_Typ.Prod, x3)]);
    let pat_triple = (x1, x2, x3) =>
      UHPat.(pat_opseq(var(x1), [(Comma, var(x2)), (Comma, var(x3))]));
    let scalar =
      UHExp.floatlit'(
        float_of_int(slider_value) /. float_of_int(slider_max),
      );
    let interpolate_vars = (x1, x2) =>
      UHExp.(
        Parenthesized(
          Block.wrap'(
            exp_opseq(
              var(x1),
              [
                (FPlus, scalar),
                (
                  FTimes,
                  Parenthesized(
                    Block.wrap'(exp_opseq(var(x2), [(FMinus, var(x1))])),
                  ),
                ),
              ],
            ),
          ),
        )
      );
    let interpolate =
      UHExp.lam(
        UHPat.(
          pat_opseq(
            Parenthesized(pat_triple("r1", "g1", "b1")),
            [(Comma, Parenthesized(pat_triple("r2", "g2", "b2")))],
          )
        ),
        ~ann=
          UHTyp.(
            typ_opseq(
              Parenthesized(typ_triple(Float, Float, Float)),
              [
                (
                  Operators_Typ.Prod,
                  Parenthesized(typ_triple(Float, Float, Float)),
                ),
              ],
            )
          ),
        UHExp.(
          Block.wrap'(
            exp_opseq(
              interpolate_vars("r1", "r2"),
              [
                (Comma, interpolate_vars("g1", "g2")),
                (Comma, interpolate_vars("b1", "b2")),
              ],
            ),
          )
        ),
      );
    UHExp.(
      Block.wrap'(
        exp_opseq(
          interpolate,
          [
            (
              Space,
              Parenthesized(
                Block.wrap'(
                  exp_opseq(
                    _to_uhvar(lcolor),
                    [(Comma, _to_uhvar(rcolor))],
                  ),
                ),
              ),
            ),
          ],
        ),
      )
    );
  };
};

module CheckboxLivelit: LIVELIT = {
  let name = "$checkbox";
  let expansion_ty = HTyp.Bool;
  let param_tys = [];

  [@deriving sexp]
  type model = bool;
  [@deriving sexp]
  type action =
    | Toggle;
  type trigger = action => Event.t;
  type sync = action => unit;

  let init_model = SpliceGenCmd.return(false);
  let update = (m, Toggle) => SpliceGenCmd.return(!m);

  let view = (m, trig, _) => {
    let checked_state = m ? [Attr.checked] : [];
    let input_elt =
      Node.input(
        [
          Attr.type_("checkbox"),
          Attr.on_input((_, _) => trig(Toggle)),
          ...checked_state,
        ],
        [],
      );
    _ => Node.span([], [input_elt]);
  };

  let view_shape = _ => LivelitShape.Inline(/* TODO! */ 1);

  let expand = m => UHExp.Block.wrap(UHExp.BoolLit(NotInHole, m));
};

module SliderLivelitMin: LIVELIT = {
  let name = "$slidem";
  let expansion_ty = HTyp.Int;
  let param_tys = [];

  [@deriving sexp]
  type model = int;
  [@deriving sexp]
  type action = int;
  type trigger = action => Event.t;
  type sync = action => unit;

  let init_model = SpliceGenCmd.return(0);
  let update = (_, n) => SpliceGenCmd.return(n);
  let view_shape = _ => LivelitShape.Inline(14);
  let expand = n => UHExp.Block.wrap(UHExp.intlit'(n));

  let view = (model, trigger: trigger, _sync, _) => {
    let value = string_of_int(model);
    let on_input = (_, value_str) => trigger(int_of_string(value_str));
    Node.span(
      [Attr.classes(["slider-livelit"])],
      [
        Node.input(
          [
            Attr.classes(["slider"]),
            Attr.type_("range"),
            Attr.create("min", "0"),
            Attr.create("max", "100"),
            Attr.value(value),
            Attr.on_input(on_input),
          ],
          [],
        ),
      ],
    );
  };
};

module SliderLivelit: LIVELIT = {
  let name = "$slider";
  let expansion_ty = HTyp.Int;
  let param_tys = [("min", HTyp.Int), ("max", HTyp.Int)];

  [@deriving sexp]
  type endpoint =
    | Min
    | Max;

  [@deriving sexp]
  type model = option(int);

  [@deriving sexp]
  type action =
    | InvalidParams
    | Slide(int);
  type trigger = action => Event.t;
  type sync = action => unit;

  let init_model = SpliceGenCmd.return(Some(0));

  let update = (_, a) =>
    switch (a) {
    | InvalidParams => SpliceGenCmd.return(None)
    | Slide(n) => SpliceGenCmd.return(Some(n))
    };

  let view = (model, trigger: trigger, sync) => {
    let _endpoint_view = (cls, value) => {
      let padding = "3px";
      let val_str = string_of_int(value);
      Node.label(
        [
          Attr.classes([cls]),
          attr_style(
            StringUtil.sep([
              prop_val("padding", padding),
              prop_val(
                "width",
                Printf.sprintf(
                  "calc(%dch + %s)",
                  String.length(val_str),
                  padding,
                ),
              ),
            ]),
          ),
        ],
        [Node.text(val_str)],
      );
    };

    let _tickmarks = (min: float, max: float) => {
      let val_of_percent = (p: int): string => {
        let p = Float.of_int(p) /. 100.0;
        Printf.sprintf("%f", (1. -. p) *. min +. p *. max);
      };
      Node.create(
        "datalist",
        [Attr.id("tickmarks")],
        [
          Node.option(
            [
              Attr.create("value", val_of_percent(0)),
              Attr.create("label", "0%"),
            ],
            [],
          ),
          Node.option([Attr.create("value", val_of_percent(10))], []),
          Node.option([Attr.create("value", val_of_percent(20))], []),
          Node.option([Attr.create("value", val_of_percent(30))], []),
          Node.option([Attr.create("value", val_of_percent(40))], []),
          Node.option(
            [
              Attr.create("value", val_of_percent(50)),
              Attr.create("label", "50%"),
            ],
            [],
          ),
          Node.option([Attr.create("value", val_of_percent(60))], []),
          Node.option([Attr.create("value", val_of_percent(70))], []),
          Node.option([Attr.create("value", val_of_percent(80))], []),
          Node.option([Attr.create("value", val_of_percent(90))], []),
          Node.option(
            [
              Attr.create("value", val_of_percent(100)),
              Attr.create("label", "100%"),
            ],
            [],
          ),
        ],
      );
    };

    let slider =
        (~disabled: bool, ~min: int=0, ~max: int=100, ~value: int=50, ()) =>
      Node.span(
        [Attr.classes(["slider-livelit"])],
        [
          // tickmarks(min, max),
          Node.input(
            [
              Attr.classes(["slider"]),
              Attr.type_("range"),
              Attr.create("min", string_of_int(min)),
              Attr.create("max", string_of_int(max)),
              // Attr.create("step", "0.01"),
              Attr.value(string_of_int(value)),
              Attr.on_input((_, value_str) =>
                trigger(Slide(int_of_string(value_str)))
              ),
              ...disabled ? [Attr.disabled] : [],
            ],
            [],
          ),
        ],
      );
    ({dargs, _}: LivelitView.splice_and_param_getters) => {
      switch (dargs) {
      | Some([
          ("min", Some((DHExp.IntLit(min), _))),
          ("max", Some((DHExp.IntLit(max), _))),
        ])
          when min <= max =>
        let value =
          switch (model) {
          | Some(n) when min <= n && n <= max => n
          | _ =>
            let new_value = (min + max) / 2;
            sync(Slide(new_value));
            new_value;
          };
        slider(~disabled=false, ~min, ~max, ~value, ());
      | _ =>
        switch (model) {
        | None => ()
        | Some(_) => sync(InvalidParams)
        };
        slider(~disabled=true, ());
      };
    };
  };

  let view_shape = _ => LivelitShape.Inline(14);

  let expand =
    fun
    | None => UHExp.Block.wrap(UHExp.intlit'(0))
    | Some(n) => UHExp.Block.wrap(UHExp.intlit'(n));
};

module DataFrameLivelit: LIVELIT = {
  let name = "$data_frame";
  let expansion_ty =
    HTyp.(Prod([List(String), List(Prod([String, List(Float)]))]));
  let param_tys = [];

  [@deriving sexp]
  type row = {
    header: SpliceName.t,
    cells: list(SpliceName.t),
  };
  // assume nonzero height and width
  [@deriving sexp]
  type model = {
    selected: SpliceName.t,
    col_headers: list(SpliceName.t),
    rows: list(row),
  };

  [@deriving sexp]
  type dim =
    | Row
    | Col;
  [@deriving sexp]
  type action =
    | Select(SpliceName.t)
    | Add(dim)
    | Del(dim, int);
  type trigger = action => Event.t;
  type sync = action => unit;

  let init_height = 3;
  let init_width = 5;

  let get_height = (m: model): int => List.length(m.rows);
  let get_width = (m: model): int => List.length(m.col_headers);

  let init_model =
    SpliceGenCmd.(
      MonadsUtil.bind_count(
        init_height,
        bind(
          MonadsUtil.bind_count(
            init_width, bind(new_splice(HTyp.Float)), row_cells =>
            bind(new_splice(HTyp.String), row_header =>
              return({header: row_header, cells: row_cells})
            )
          ),
        ),
        rows =>
        MonadsUtil.bind_count(
          init_width, bind(new_splice(HTyp.String)), col_headers =>
          return({selected: List.hd(col_headers), col_headers, rows})
        )
      )
    );

  let update = (m, u): SpliceGenCmd.t(model) =>
    switch (u) {
    | Select(splice) => SpliceGenCmd.return({...m, selected: splice})
    | Add(Row) =>
      SpliceGenCmd.(
        MonadsUtil.bind_count(
          get_width(m), bind(new_splice(HTyp.Float)), new_cells =>
          bind(new_splice(HTyp.String), new_header =>
            return({
              ...m,
              selected: new_header,
              rows: m.rows @ [{header: new_header, cells: new_cells}],
            })
          )
        )
      )
    | Add(Col) =>
      SpliceGenCmd.(
        MonadsUtil.bind_count(
          get_height(m), bind(new_splice(HTyp.Float)), new_cells =>
          bind(new_splice(HTyp.String), new_header =>
            return({
              selected: new_header,
              col_headers: m.col_headers @ [new_header],
              rows:
                List.map2(
                  (r, c) => {...r, cells: r.cells @ [c]},
                  m.rows,
                  new_cells,
                ),
            })
          )
        )
      )
    | Del(Row, i) =>
      switch (ListUtil.split_nth_opt(i, m.rows)) {
      | None
      | Some(([], _, [])) => SpliceGenCmd.return(m)
      | Some((prefix, row_to_drop, suffix)) =>
        let row_to_drop = [row_to_drop.header, ...row_to_drop.cells];
        let selected =
          List.exists((==)(m.selected), row_to_drop)
            ? switch (ListUtil.split_first_opt(suffix)) {
              | Some((hd, _)) => hd.header
              | None =>
                let (_, last) = ListUtil.split_last(prefix);
                last.header;
              }
            : m.selected;
        SpliceGenCmd.(
          MonadsUtil.bind_list(
            row_to_drop,
            splice => bind(drop_splice(splice)),
            _ => return({...m, selected, rows: prefix @ suffix}),
          )
        );
      }
    | Del(Col, i) =>
      if (get_width(m) <= 1) {
        SpliceGenCmd.return(m);
      } else {
        let (hdr_prefix, hdr_to_drop, hdr_suffix) =
          ListUtil.split_nth(i, m.col_headers);
        let (prefixes, cells_to_drop, suffixes) =
          m.rows
          |> List.map(row => row.cells)
          |> List.map(ListUtil.split_nth(i))
          |> ListUtil.split3;
        let col_to_drop = [hdr_to_drop, ...cells_to_drop];
        let selected =
          List.exists((==)(m.selected), col_to_drop)
            ? {
              let col =
                switch (ListUtil.split_first_opt(suffixes)) {
                | Some((hd, _)) => hd
                | None =>
                  let (_, last) = ListUtil.split_last(prefixes);
                  last;
                };
              List.hd(col);
            }
            : m.selected;
        let col_headers = hdr_prefix @ hdr_suffix;
        let rows = {
          let row_hdrs = List.map(row => row.header, m.rows);
          List.combine(row_hdrs, List.combine(prefixes, suffixes))
          |> List.map(((header, (prefix, suffix))) =>
               {header, cells: prefix @ suffix}
             );
        };
        SpliceGenCmd.(
          MonadsUtil.bind_list(
            col_to_drop,
            splice => bind(drop_splice(splice)),
            _ => return({selected, col_headers, rows}),
          )
        );
      }
    };

  let grid_area = ((row_start, col_start, row_end, col_end)) =>
    prop_val(
      "grid-area",
      Printf.sprintf(
        "%d / %d / %d / %d",
        row_start,
        col_start,
        row_end,
        col_end,
      ),
    );

  let view =
      (
        m: model,
        trig,
        _,
        {uhcode, dhcode, _}: LivelitView.splice_and_param_getters,
      ) => {
    let splice = (~clss, ~grid_coordinates, splice_name) =>
      Node.div(
        [
          attr_style(grid_area(grid_coordinates)),
          Attr.classes([
            "matrix-splice",
            splice_name == m.selected
              ? "matrix-selected" : "matrix-unselected",
            ...clss,
          ]),
          Attr.on_mousedown(_ => trig(Select(splice_name))),
          // Attr.on_click(_ => trig(Del(Col, j))),
        ],
        [
          switch (dhcode(splice_name)) {
          | None => Node.text("-")
          | Some((_, view)) => view
          },
          //Node.span([Attr.classes(["delete"])], [Node.text("x")]),
        ],
      );

    let width = get_width(m);
    let height = get_height(m);
    let col_headers =
      m.col_headers
      |> List.mapi((j, header) =>
           splice(
             ~clss=["col-header"],
             ~grid_coordinates=(1, j + 2, 2, j + 3),
             header,
           )
         );
    let row_headers =
      m.rows
      |> List.mapi((i, row) =>
           splice(
             ~clss=["row-header"],
             ~grid_coordinates=(i + 2, 1, i + 3, 2),
             row.header,
           )
         );
    let cells =
      m.rows
      |> List.mapi((i, row) =>
           row.cells
           |> List.mapi((j, cell) =>
                splice(
                  ~clss=["matrix-cell"],
                  ~grid_coordinates=(i + 2, j + 2, i + 3, j + 3),
                  cell,
                )
              )
         )
      |> List.flatten;

    let add_row_button =
      Node.button(
        [
          attr_style(grid_area(((-1), 2, (-2), (-3)))),
          Attr.classes(["add-row", "add-button"]),
          Attr.on_click(_ => trig(Add(Row))),
        ],
        [Node.text("+")],
      );
    let add_col_button =
      Node.button(
        [
          attr_style(grid_area((2, (-2), (-3), (-1)))),
          Attr.classes(["add-col", "add-button"]),
          Attr.on_click(_ => trig(Add(Col))),
        ],
        [Node.text("+")],
      );

    let cells_border =
      Node.div(
        [
          attr_style(grid_area((2, 2, (-3), (-3)))),
          Attr.classes(["cells-border"]),
        ],
        [],
      );

    let dim_template = dim =>
      StringUtil.sep(
        List.concat([ListUtil.replicate(1 + dim, "auto"), ["4px", "auto"]]),
      );

    Node.div(
      [Attr.classes(["matrix-livelit"])],
      [
        Node.div(
          [Attr.classes(["formula-bar"])],
          [
            Node.div(
              [Attr.classes(["formula-bar-prompt"])],
              [Node.text(" > ")],
            ),
            uhcode(m.selected),
          ],
        ),
        Node.div(
          [
            Attr.classes(["grid-container"]),
            attr_style(
              StringUtil.cat([
                prop_val("grid-template-columns", dim_template(width)),
                prop_val("grid-template-rows", dim_template(height)),
              ]),
            ),
          ],
          List.concat([
            row_headers,
            col_headers,
            [cells_border, ...cells],
            [add_row_button, add_col_button],
          ]),
        ),
      ],
    );
  };

  let view_shape = m => {
    LivelitShape.MultiLine(3 * get_height(m) + 1);
  };

  let expand = m => {
    let to_uhexp_list =
      fun
      | [] => Seq.wrap(UHExp.listnil())
      | [fst, ...rest] => {
          let rest' =
            (rest |> List.map(item => (Operators_Exp.Cons, item)))
            @ [(Operators_Exp.Cons, UHExp.listnil())];
          Seq.mk(fst, rest');
        };
    let col_headers = m.col_headers |> List.map(_to_uhvar) |> to_uhexp_list;
    let rows =
      m.rows
      |> List.map(row => {
           let header = _to_uhvar(row.header);
           let cells = row.cells |> List.map(_to_uhvar) |> to_uhexp_list;
           UHExp.(
             Parenthesized([
               ExpLine(
                 UHExp.mk_OpSeq(
                   Seq.S(header, A(Operators_Exp.Comma, cells)),
                 ),
               ),
             ])
           );
         })
      |> to_uhexp_list;
    UHExp.[
      ExpLine(
        mk_OpSeq(Seq.seq_op_seq(col_headers, Operators_Exp.Comma, rows)),
      ),
    ];
  };
};

/* ----------
   stuff below is infrastructure
   ---------- */

module GradientLivelitAdapter = LivelitAdapter(GradientLivelit);
module CheckboxLivelitAdapter = LivelitAdapter(CheckboxLivelit);
module PairLivelitAdapter = LivelitAdapter(PairLivelit);
module SliderLivelitAdapter = LivelitAdapter(SliderLivelit);
module SliderLivelitMinAdapter = LivelitAdapter(SliderLivelitMin);
module MatrixLivelitAdapter = LivelitAdapter(MatrixLivelit);
module LiveMatrixLivelitAdapter = LivelitAdapter(LiveMatrixLivelit);
module GradeCutoffLivelitAdapter = LivelitAdapter(GradeCutoffLivelit);
module DataFrameLivelitAdapter = LivelitAdapter(DataFrameLivelit);
module GrayscaleLivelitAdapter = LivelitAdapter(GrayscaleLivelit);
let empty_livelit_contexts = LivelitContexts.empty;
let (initial_livelit_ctx, initial_livelit_view_ctx) =
  LivelitContexts.extend(
    LivelitContexts.extend(
      LivelitContexts.extend(
        LivelitContexts.extend(
          LivelitContexts.extend(
            LivelitContexts.extend(
              LivelitContexts.extend(
                LivelitContexts.extend(
                  LivelitContexts.extend(
                    LivelitContexts.extend(
                      LivelitContexts.extend(
                        empty_livelit_contexts,
                        GrayscaleLivelitAdapter.contexts_entry,
                      ),
                      DataFrameLivelitAdapter.contexts_entry,
                    ),
                    GradeCutoffLivelitAdapter.contexts_entry,
                  ),
                  MatrixLivelitAdapter.contexts_entry,
                ),
                LiveMatrixLivelitAdapter.contexts_entry,
              ),
              PairLivelitAdapter.contexts_entry,
            ),
            CheckboxLivelitAdapter.contexts_entry,
          ),
          SliderLivelitAdapter.contexts_entry,
        ),
        ColorLivelitAdapter.contexts_entry,
      ),
      GradientLivelitAdapter.contexts_entry,
    ),
    SliderLivelitMinAdapter.contexts_entry,
  );
