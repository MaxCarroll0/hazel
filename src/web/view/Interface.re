open Virtual_dom.Vdom;
open Node;
open Core;

let get_result =
    (d: Elaborator_Exp.ElaborationResult.t): option((DHExp.t, TestMap.t)) => {
  let evaluate =
    Core_kernel.Memo.general(~cache_size_bound=1000, Evaluator.evaluate);
  switch (d) {
  | Elaborates(elab, _, _) =>
    switch (elab |> evaluate) {
    | (Evaluator.BoxedValue(d), {test_map, _})
    | (Indet(d), {test_map, _}) => Some((d, test_map))
    | exception _ => None
    }
  | _ => None
  };
};

let dhcode_view = (~font_metrics: FontMetrics.t, result) => {
  DHCode.view_tylr(
    ~selected_instance=None,
    ~font_metrics={
      row_height: font_metrics.row_height,
      col_width: font_metrics.col_width,
    },
    ~settings=Settings.Evaluation.init,
    ~width=80,
    result,
  );
};

let res_view = (~font_metrics: FontMetrics.t, term, info_map): Node.t => {
  let result = get_result(Elaborator.uexp_elab(info_map, term));
  div(
    [Attr.classes(["result"])],
    switch (result) {
    | None => []
    | Some((result, _)) => [dhcode_view(~font_metrics, result)]
    },
  );
};
