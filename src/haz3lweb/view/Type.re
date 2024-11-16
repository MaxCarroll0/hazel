open Virtual_dom.Vdom;
open Node;
open Util.Web;

let ty_view = (cls: string, s: string): Node.t =>
  div(~attrs=[clss(["typ-view", cls])], [text(s)]);

let prov_view: Haz3lcore.Typ.type_provenance => Node.t =
  fun
  | Internal => div([])
  | TypeHole =>
    div(~attrs=[clss(["typ-mod", "type-hole"])], [text("𝜏")])
  | SynSwitch =>
    div(~attrs=[clss(["typ-mod", "syn-switch"])], [text("⇒")]);

let rec view = (ty: Haz3lcore.Typ.t): Node.t =>
  //TODO: parens on ops when ambiguous
  switch (ty) {
  | Unknown(prov) =>
    div(
      ~attrs=[clss(["typ-view", "atom", "unknown"])],
      [text("?"), prov_view(prov)],
    )
  | Int => ty_view("Int", "Int")
  | Float => ty_view("Float", "Float")
  | String => ty_view("String", "String")
  | Bool => ty_view("Bool", "Bool")
  | Var(name) => ty_view("Var", name)
  | List(t) =>
    div(
      ~attrs=[clss(["typ-view", "atom", "List"])],
      [text("["), view(t), text("]")],
    )
  | Arrow(t1, t2) =>
    div(
      ~attrs=[clss(["typ-view", "Arrow"])],
      [view(t1), text("->"), view(t2)],
    )
  | Prod([]) => div(~attrs=[clss(["typ-view", "Prod"])], [text("Unit")])
  | Prod([_]) =>
    div(~attrs=[clss(["typ-view", "Prod"])], [text("BadProduct")])
  | Prod([t0, ...ts]) =>
    div(
      ~attrs=[clss(["typ-view", "atom", "Prod"])],
      [
        text("("),
        div(
          ~attrs=[clss(["typ-view", "Prod"])],
          [view(t0)]
          @ (List.map(t => [text(","), view(t)], ts) |> List.flatten),
        ),
        text(")"),
      ],
    )
  | Sum(t1, t2) =>
    div(
      ~attrs=[clss(["typ-view", "Sum"])],
      [view(t1), text("+"), view(t2)],
    )
  };
