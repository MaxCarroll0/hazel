open Virtual_dom.Vdom;
open Node;
open Util.Web;

let context_entry_view =
    ((name: string, {typ, _}: Haz3lcore.Ctx.entry)): Node.t =>
  div(
    ~attr=clss(["context-entry"]),
    [text(name), text(":"), Type.view(typ)],
  );

let ctxc = "context-entries";

let exp_ctx_view = (ctx: Haz3lcore.Ctx.t): Node.t =>
  div(
    ~attr=clss([ctxc, "exp"]),
    List.map(context_entry_view, List.rev(ctx)),
  );

let pat_ctx_view = (ctx: Haz3lcore.Ctx.t): Node.t =>
  div(
    ~attr=clss([ctxc, "pat"]),
    List.map(context_entry_view, List.rev(ctx)),
  );

let ctx_sorts_view = (ci: Haz3lcore.Statics.t): Node.t => {
  switch (ci) {
  | Invalid(_) => div(~attr=clss([ctxc, "invalid"]), [text("")])
  | InfoExp({ctx, _}) => exp_ctx_view(ctx)
  | InfoPat({ctx, _}) => pat_ctx_view(ctx)
  | InfoTyp(_) => div(~attr=clss([ctxc, "typ"]), [])
  | InfoRul(_) => div(~attr=clss([ctxc, "rul"]), [])
  };
};

let inspector_view =
    (~settings: Model.settings, _id: int, ci: Haz3lcore.Statics.t): Node.t => {
  let clss =
    clss(
      ["context-inspector"] @ (settings.context_inspector ? ["visible"] : []),
    );
  div(~attr=clss, [ctx_sorts_view(ci)]);
};

let view =
    (
      ~settings: Model.settings,
      index': option(int),
      info_map: Haz3lcore.Statics.map,
    ) => {
  let (index, ci) =
    switch (index') {
    | Some(index) => (index, Haz3lcore.Id.Map.find_opt(index, info_map))
    | None => ((-1), None)
    };
  switch (ci) {
  | None =>
    div(~attr=clss(["context-inspector"]), [text("No Static Data")])
  | Some(ci) => inspector_view(~settings, index, ci)
  };
};
