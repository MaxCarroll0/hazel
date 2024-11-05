open Util;
open Virtual_dom.Vdom;
open ProjectorBase;

let of_id = (id: Id.t) =>
  "id" ++ (id |> Id.to_string |> String.sub(_, 0, 8));

let of_mono = (syntax: Piece.t): option(string) =>
  switch (syntax) {
  | Tile({label: [l], _}) => Some(StringUtil.unescape_linebreaks(l))
  | _ => None
  };

let mk_mono = (sort: Sort.t, string: string): Piece.t =>
  string
  |> StringUtil.escape_linebreaks
  |> Form.mk_atomic(sort)
  |> Piece.mk_tile(_, []);

let get = (piece: Piece.t): string =>
  switch (piece |> of_mono) {
  | None => failwith("TextArea: not string literal")
  | Some(s) => s
  };

let put = (s: string): Piece.t => s |> mk_mono(Exp);

let put = (str: string): external_action =>
  SetSyntax(str |> Form.string_quote |> put);

let is_last_pos = id =>
  Web.TextArea.caret_at_end(Web.TextArea.get(of_id(id)));
let is_first_pos = id =>
  Web.TextArea.caret_at_start(Web.TextArea.get(of_id(id)));

let key_handler = (id, ~parent, evt) => {
  open Effect;
  let key = Key.mk(KeyDown, evt);

  switch (key.key) {
  | D("ArrowRight" | "ArrowDown") when is_last_pos(id) =>
    JsUtil.get_elem_by_id(of_id(id))##blur;
    Many([parent(Escape(Right)), Stop_propagation]);
  | D("ArrowLeft" | "ArrowUp") when is_first_pos(id) =>
    JsUtil.get_elem_by_id(of_id(id))##blur;
    Many([parent(Escape(Left)), Stop_propagation]);
  /* Defer to parent editor undo for now */
  | D("z" | "Z" | "y" | "Y") when Key.ctrl_held(evt) || Key.meta_held(evt) =>
    Many([Prevent_default])
  | D("z" | "Z")
      when Key.shift_held(evt) && (Key.ctrl_held(evt) || Key.meta_held(evt)) =>
    Many([Prevent_default])
  | D("\"") =>
    /* Hide quotes from both the textarea and parent editor */
    Many([Prevent_default, Stop_propagation])
  | _ => Stop_propagation
  };
};

let safe_html_to_node = (html_string: string): Node.t =>
  Node.div(~attrs=[Attr.create("innerHTML", html_string)], []);
let textarea =
    (
      id,
      ~parent as _: external_action => Ui_effect.t(unit),
      ~font_metrics: FontMetrics.t,
      text: string,
    ) => {
  let foo = Omd.of_string(text);
  let bar = Omd.to_html(foo);
  let size =
    Css_gen.concat([
      Css_gen.overflow(`Auto),
      Css_gen.height(`Px(int_of_float(30. *. font_metrics.row_height))),
      Css_gen.width(`Px(int_of_float(150. *. font_metrics.col_width))),
    ]);
  // Node.innerHtml(bar);
  let foo =
    Node.inner_html(
      ~attrs=[Attr.id(of_id(id)), Attr.style(size)],
      ~this_html_is_sanitized_and_is_totally_safe_trust_me=bar, // ;)
      ~tag="div",
    );
  foo();
};

let view = (_, ~info, ~local as _, ~parent, ~font_metrics) => {
  let text = info.syntax |> get |> Form.strip_quotes;
  Node.div(
    ~attrs=[Attr.classes(["wrapper"])],
    [
      Node.div(
        ~attrs=[Attr.classes(["cols", "code"])],
        [Node.text("·")]
        @ [textarea(info.id, ~parent, ~font_metrics, text)],
      ),
    ],
  );
};

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = unit;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = unit;
  let init = ();
  let can_project = _ => true; //TODO(andrew): restrict somehow
  let can_focus = true;
  let placeholder = (_, _info) => {
    Block({
      row: 30,
      /* +2 for left and right padding */
      col: 150,
    });
  };
  let update = (model, _) => model;
  let view = view;
  let focus = ((id: Id.t, d: option(Direction.t))) => {
    JsUtil.get_elem_by_id(of_id(id))##focus;
    switch (d) {
    | None => ()
    | Some(Left) =>
      Web.TextArea.set_caret_to_start(Web.TextArea.get(of_id(id)))
    | Some(Right) =>
      Web.TextArea.set_caret_to_end(Web.TextArea.get(of_id(id)))
    };
  };
};
