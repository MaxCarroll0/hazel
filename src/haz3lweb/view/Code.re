open Virtual_dom.Vdom;
open Node;
open Haz3lcore;
open Util;
open Util.Web;

let of_delim' =
    (
      (
        sort,
        is_consistent,
        is_complete,
        label,
        i,
        inject,
        livelit_state: Id.Map.t(DHExp.t),
        tile_id: Id.t,
      ),
    ) => {
  let cls =
    switch (label) {
    | [_] when !is_consistent => "mono-inconsistent"
    | [s] when Form.is_string(s) => "mono-string-lit"
    | [_] => "mono"
    | _ when !is_consistent => "delim-inconsistent"
    | _ when !is_complete => "delim-incomplete"
    | _ => "delim"
    };
  // TODO FontMetrics
  let font_height = 10.0;
  let font_width = 10.0;
  let stop_mousedown_propagation =
    Attr.on_mousedown(evt => {
      Js_of_ocaml.Dom_html.stopPropagation(evt);
      Virtual_dom.Vdom.Effect.Ignore;
    });

  let livelit_node: list(t) =
    switch (label) {
    | ["^slider"] => [
        Node.input(
          ~attr=
            Attr.many([
              Attr.create("type", "range"),
              Attr.create(
                "style",
                Printf.sprintf(
                  "width: %fpx; height: %fpx;",
                  10.0 *. font_width,
                  font_height,
                ),
              ),
              Attr.create(
                "value",
                string_of_int(
                  switch (Id.Map.find_opt(tile_id, livelit_state)) {
                  | Some(IntLit(i)) => i
                  | _ => 50
                  },
                ),
              ),
              Attr.on_input((_evt, str) =>
                (
                  {
                    inject(
                      UpdateAction.LivelitStateChange(
                        tile_id,
                        IntLit(int_of_string(str)),
                      ),
                    );
                  }:
                    Virtual_dom.Vdom.Effect.t(unit)
                )
              ),
              stop_mousedown_propagation,
            ]),
          (),
        ),
      ]
    | ["^checkbox"] =>
      let checkbox_state: bool =
        switch (Id.Map.find_opt(tile_id, livelit_state)) {
        | Some(BoolLit(b)) => b
        | _ => false
        };
      let checkbox_callback: Attr.t =
        Attr.on_change((_evt, _str) => {
          inject(
            UpdateAction.LivelitStateChange(
              tile_id,
              BoolLit(!checkbox_state),
            ),
          )
        });

      [
        Node.input(
          ~attr=
            Attr.many([
              Attr.create("type", "checkbox"),
              Attr.create(
                "style",
                Printf.sprintf(
                  "width: %fpx; height: %fpx; margin: 0",
                  1.0 *. font_width,
                  font_height,
                ),
              ),
              checkbox_state ? Attr.checked : Attr.create("foo", "bar"),
              checkbox_callback,
              stop_mousedown_propagation,
            ]),
          (),
        ),
      ];
    | _ => []
    };
  [
    span(
      ~attr=Attr.classes(["token", cls, "text-" ++ Sort.to_string(sort)]),
      List.append([Node.text(List.nth(label, i))], livelit_node),
    ),
  ];
};

let of_delim =
    (
      sort: Sort.t,
      is_consistent,
      t: Piece.tile,
      i: int,
      ~inject,
      ~livelit_state: Id.Map.t(DHExp.t),
    )
    : list(Node.t) =>
  of_delim'((
    sort,
    is_consistent,
    Tile.is_complete(t),
    t.label,
    i,
    inject,
    livelit_state,
    Tile.id(t),
  ));

let of_grout = [Node.text(Unicode.nbsp)];

let of_secondary =
  Core.Memo.general(
    ~cache_size_bound=1000000, ((secondary_icons, indent, content)) =>
    if (String.equal(Secondary.get_string(content), Secondary.linebreak)) {
      let str = secondary_icons ? Secondary.linebreak : "";
      [
        span_c("linebreak", [text(str)]),
        Node.br(),
        Node.text(StringUtil.repeat(indent, Unicode.nbsp)),
      ];
    } else if (String.equal(Secondary.get_string(content), Secondary.space)) {
      let str = secondary_icons ? "·" : Unicode.nbsp;
      [span_c("secondary", [text(str)])];
    } else if (Secondary.content_is_comment(content)) {
      [span_c("comment", [Node.text(Secondary.get_string(content))])];
    } else {
      [span_c("secondary", [Node.text(Secondary.get_string(content))])];
    }
  );

module Text = (M: {
                 let map: Measured.t;
                 let settings: Model.settings;
               }) => {
  let m = p => Measured.find_p(p, M.map);
  let rec of_segment =
          (
            ~no_sorts=false,
            ~sort=Sort.root,
            seg: Segment.t,
            ~inject,
            ~livelit_state: Id.Map.t(DHExp.t),
          )
          : list(Node.t) => {
    //note: no_sorts flag is used for backback
    let expected_sorts: list((int, Sort.t)) =
      no_sorts
        ? List.init(List.length(seg), i => (i, Sort.Any))
        : Segment.expected_sorts(sort, seg);
    let sort_of_p_idx: int => Sort.t =
      idx =>
        switch (List.assoc_opt(idx, expected_sorts)) {
        | None => Sort.Any
        | Some(sort) => sort
        };
    seg
    |> List.mapi((i, p) => (i, p))
    |> List.concat_map(((i, p)) =>
         of_piece(sort_of_p_idx(i), p, ~inject, ~livelit_state)
       );
  }
  and of_piece =
      (expected_sort: Sort.t, p: Piece.t, ~inject, ~livelit_state)
      : list(Node.t) => {
    switch (p) {
    | Tile(t) => of_tile(expected_sort, t, ~inject, ~livelit_state)
    | Grout(_) => of_grout
    | Secondary({content, _}) =>
      of_secondary((M.settings.secondary_icons, m(p).last.col, content))
    };
  }
  and of_tile =
      (expected_sort: Sort.t, t: Tile.t, ~inject, ~livelit_state)
      : list(Node.t) => {
    let children_and_sorts =
      List.mapi(
        (i, (l, child, r)) =>
          //TODO(andrew): more subtle logic about sort acceptability
          (child, l + 1 == r ? List.nth(t.mold.in_, i) : Sort.Any),
        Aba.aba_triples(Aba.mk(t.shards, t.children)),
      );
    let is_consistent = Sort.consistent(t.mold.out, expected_sort);
    let foo = Aba.mk(t.shards, children_and_sorts);
    foo
    |> Aba.join(
         of_delim(t.mold.out, is_consistent, t, ~inject, ~livelit_state),
         ((seg, sort)) =>
         of_segment(~sort, seg, ~inject, ~livelit_state)
       )
    |> List.concat;
  };
};

let rec holes =
        (~font_metrics, ~map: Measured.t, seg: Segment.t): list(Node.t) =>
  seg
  |> List.concat_map(
       fun
       | Piece.Secondary(_) => []
       | Tile(t) => List.concat_map(holes(~map, ~font_metrics), t.children)
       | Grout(g) => [
           EmptyHoleDec.view(
             ~font_metrics, // TODO(d) fix sort
             {
               measurement: Measured.find_g(g, map),
               mold: Mold.of_grout(g, Any),
             },
           ),
         ],
     );

let simple_view =
    (~unselected, ~map, ~settings: Model.settings, ~inject): Node.t => {
  module Text =
    Text({
      let map = map;
      let settings = settings;
    });
  div(
    ~attr=Attr.class_("code"),
    [
      span_c(
        "code-text",
        Text.of_segment(unselected, ~inject, ~livelit_state=Id.Map.empty),
      ),
    ] // TODO livelit_state
  );
};

let view =
    (
      ~font_metrics,
      ~segment,
      ~unselected,
      ~measured,
      ~settings: Model.settings,
      ~inject,
      ~livelit_state: Id.Map.t(DHExp.t),
    )
    : Node.t => {
  module Text =
    Text({
      let map = measured;
      let settings = settings;
    });
  let unselected: list(t) =
    TimeUtil.measure_time("Code.view/unselected", settings.benchmark, () =>
      Text.of_segment(unselected, ~inject, ~livelit_state)
    );
  let holes =
    TimeUtil.measure_time("Code.view/holes", settings.benchmark, () =>
      holes(~map=measured, ~font_metrics, segment)
    );
  div(
    ~attr=Attr.class_("code"),
    [
      span_c("code-text", unselected),
      // TODO restore (already regressed so no loss in commenting atm)
      // span_c("code-text-shards", Text.of_segment(segment)),
      ...holes,
    ],
  );
};
