open Virtual_dom.Vdom;
open Node;
open Util.Web;
open Util;
open Haz3lcore;

let errc = "error";
let okc = "ok";
let div_err = div(~attr=clss([errc]));
let div_ok = div(~attr=clss([okc]));

let cls_str = (ci: Statics.Info.t): string =>
  switch (ci) {
  | InfoExp({cls, _}) => Term.UExp.show_cls(cls)
  | InfoPat({cls, _}) => Term.UPat.show_cls(cls)
  | InfoTyp({cls, _}) => Term.UTyp.show_cls(cls)
  | InfoTPat({cls, _}) => Term.UTPat.show_cls(cls)
  };

let lang_doc_toggle = (~inject, ~show_lang_doc) => {
  let tooltip = "Toggle language documentation";
  let toggle_landocs = _ =>
    Virtual_dom.Vdom.Effect.Many([
      inject(Update.UpdateLangDocMessages(LangDocMessages.ToggleShow)),
      Virtual_dom.Vdom.Effect.Stop_propagation,
    ]);
  div(
    ~attr=clss(["lang-doc-button"]),
    [Widgets.toggle(~tooltip, "i", show_lang_doc, toggle_landocs)],
  );
};

let term_tag =
    (
      ~inject,
      ~settings: ModelSettings.t,
      ~show_lang_doc,
      is_err,
      sort,
      id,
      ci,
    ) =>
  div(
    ~attr=
      clss(["ci-header", "ci-header-" ++ sort] @ (is_err ? [errc] : [])),
    [
      div(
        ~attr=
          Attr.many([
            Attr.on_click(_ => inject(Update.Set(ContextInspector))),
            clss(
              ["gamma"] @ (settings.context_inspector ? ["visible"] : []),
            ),
          ]),
        [text("Γ")],
      ),
      CtxInspector.inspector_view(~inject, ~settings, id, ci),
      div(~attr=clss(["term-tag"]), [text(sort)]),
      lang_doc_toggle(~inject, ~show_lang_doc),
    ],
  );

let common_err_view = (err: Info.error_common) =>
  switch (err) {
  | BadToken(token) => [
      text(Printf.sprintf("\"%s\" isn't a valid token", token)),
    ]
  | InconsistentWithArrow(typ) => [
      Type.view(typ),
      text("is not consistent with arrow type"),
    ]
  | FreeTag => [text("Constructor is not defined")]
  | SynInconsistentBranches(tys) => [
      text("Expecting branches to have consistent types but got:"),
      ...ListUtil.join(text(","), List.map(Type.view, tys)),
    ]
  | TypeInconsistent({ana, syn}) => [
      text("Expecting"),
      Type.view(ana),
      text("but got"),
      Type.view(syn),
    ]
  };

let common_ok_view = (ok: Info.ok_pat) => {
  switch (ok) {
  | SynConsistent(ty_syn) => [text("has type"), Type.view(ty_syn)]
  | AnaConsistent({ana, syn, _}) when ana == syn => [
      text("has expected & actual type"),
      Type.view(ana),
    ]
  | AnaConsistent({ana, syn: Unknown(_), _}) => [
      text("satisfies expected type"),
      Type.view(ana),
    ]
  | AnaConsistent({ana, syn, _}) => [
      text("has type"),
      Type.view(syn),
      text("which is consistent with"),
      Type.view(ana),
    ]
  | AnaInternalInconsistent({ana, nojoin}) =>
    [
      text("is consistent with"),
      Type.view(ana),
      text("but is internally inconsistent:"),
    ]
    @ ListUtil.join(text(","), List.map(Type.view, nojoin))
  };
};

let typ_ok_view = (ok: Info.ok_typ) =>
  switch (ok) {
  | Variant(name, sum_ty) => [
      Type.view(Var(name)),
      text("is a sum type constuctor of type"),
      Type.view(sum_ty),
    ]
  | VariantIncomplete(sum_ty) => [
      text("An incomplete sum type constuctor of type"),
      Type.view(sum_ty),
    ]
  | Type(ty) => [Type.view(ty), text("is a type")]
  | TypeAlias(name, ty_lookup) => [
      Type.view(Var(name)),
      text("is a type alias for"),
      Type.view(ty_lookup),
    ]
  };

let typ_err_view = (ok: Info.error_typ) =>
  switch (ok) {
  | FreeTypeVar(name) => [
      text("Type variable"),
      Type.view(Var(name)),
      text("is not bound"),
    ]
  | BadToken(token) => [
      text(Printf.sprintf("\"%s\" isn't a valid type token", token)),
    ]
  | WantTagFoundAp => [text("Expected a constructor, found application")]
  | WantTagFoundType(ty) => [
      text("Expected a constructor, found type "),
      Type.view(ty),
    ]
  | WantTypeFoundAp => [text("Constructor application must be in sum")]
  | DuplicateTag(name) => [
      text("Constructor"),
      Type.view(Var(name)),
      text("already used in this sum"),
    ]
  };

let exp_view: Info.status_exp => t =
  fun
  | InHole(FreeVariable) => div_err([text("Variable is not bound")])
  | InHole(Common(error)) => div_err(common_err_view(error))
  | NotInHole(ok) => div_ok(common_ok_view(ok));

let pat_view: Info.status_pat => t =
  fun
  | InHole(ExpectedTag) => div_err([text("Expected a constructor")])
  | InHole(Common(error)) => div_err(common_err_view(error))
  | NotInHole(ok) => div_ok(common_ok_view(ok));

let typ_view: Info.status_typ => t =
  fun
  | NotInHole(ok) => div_ok(typ_ok_view(ok))
  | InHole(err) => div_err(typ_err_view(err));

let tpat_view: Info.status_tpat => t =
  fun
  | NotInHole(Empty) => div_ok([text("Enter a new type alias")])
  | NotInHole(Var(name)) =>
    div_ok([Type.alias_view(name), text("is a new type alias")])
  | InHole(NotAVar) => div_err([text("Not a valid type name")])
  | InHole(ShadowsType(name)) =>
    div_err([
      text("Can't shadow existing alias or base type"),
      Type.view(Var(name)),
    ]);

let view_of_info =
    (~inject, ~settings, ~show_lang_doc: bool, id, ci: Statics.Info.t): Node.t => {
  let wrapper = (sort, status_view) =>
    div(
      ~attr=clss(["info", sort]),
      [
        term_tag(
          ~inject,
          ~settings,
          ~show_lang_doc,
          Info.is_error(ci),
          sort,
          id,
          ci,
        ),
        status_view,
      ],
    );
  switch (ci) {
  | InfoExp({status, _}) => wrapper("exp", exp_view(status))
  | InfoPat({status, _}) => wrapper("pat", pat_view(status))
  | InfoTyp({status, _}) => wrapper("typ", typ_view(status))
  | InfoTPat({status, _}) => wrapper("tpat", tpat_view(status))
  };
};

let cls_and_id_view = (id: int, ci: Statics.Info.t): Node.t =>
  div(
    ~attr=Attr.many([clss(["id-and-class"])]),
    [
      div(~attr=clss(["syntax-class"]), [text(cls_str(ci))]),
      div(~attr=clss(["id"]), [text(string_of_int(id + 1))]),
    ],
  );

let inspector_view = (~inject, ~settings, ~show_lang_doc, id, ci): Node.t =>
  div(
    ~attr=clss(["cursor-inspector"] @ [Info.is_error(ci) ? errc : okc]),
    [view_of_info(~inject, ~settings, ~show_lang_doc, id, ci)],
  );

let view =
    (
      ~inject,
      ~settings: ModelSettings.t,
      ~show_lang_doc: bool,
      zipper: Zipper.t,
      info_map: Statics.Map.t,
    ) => {
  let bar_view = div_c("bottom-bar");
  let err_view = err =>
    bar_view([
      div(
        ~attr=clss(["cursor-inspector", "no-info"]),
        [div(~attr=clss(["icon"]), [Icons.magnify]), text(err)],
      ),
    ]);
  switch (zipper.backpack, Indicated.index(zipper)) {
  | ([_, ..._], _) => err_view("No information while backpack in use")
  | (_, None) => err_view("No cursor in program")
  | (_, Some(id)) =>
    switch (Id.Map.find_opt(id, info_map)) {
    | None => err_view("Whitespace or Comment")
    | Some(ci) =>
      bar_view([
        inspector_view(~inject, ~settings, ~show_lang_doc, id, ci),
        cls_and_id_view(id, ci),
      ])
    }
  };
};
