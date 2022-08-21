let evaluate =
  Core_kernel.Memo.general(~cache_size_bound=1000, Evaluator.evaluate);

let convert_metrics = (font_metrics: FontMetrics.t): DHCode.font_metrics => {
  row_height: font_metrics.row_height,
  col_width: font_metrics.col_width,
};

let dhcode_view = (~font_metrics: FontMetrics.t) => {
  DHCode.view_tylr(
    ~selected_instance=None, //option((int, int)) // hole, hole_inst
    ~font_metrics=convert_metrics(font_metrics),
    ~settings=Settings.Evaluation.init,
  );
};

let get_result =
    (d: Elaborator_Exp.ElaborationResult.t): option((DHExp.t, TestMap.t)) => {
  switch (d) {
  | Elaborates(elab, _, _) =>
    switch (elab |> evaluate) {
    | (EvaluatorResult.BoxedValue(d), {test_map, _})
    | (Indet(d), {test_map, _}) => Some((d, List.rev(test_map)))
    | exception _ => None
    }
  | _ => None
  };
};

let evaulation_result = (map, term): option(DHExp.t) =>
  switch (Core.Elaborator.uexp_elab(map, term) |> get_result) {
  | None => None
  | Some((result, _)) => Some(result)
  };

type test_results = {
  test_map: TestMap.t,
  statuses: list(TestStatus.t),
  descriptions: list(string),
  total: int,
  passing: int,
  failing: int,
  unfinished: int,
};

let mk_results = (~descriptions=[], test_map: TestMap.t): test_results => {
  test_map,
  statuses: test_map |> List.map(r => r |> snd |> TestMap.joint_status),
  descriptions,
  total: TestMap.count(test_map),
  passing: TestMap.count_status(Pass, test_map),
  failing: TestMap.count_status(Fail, test_map),
  unfinished: TestMap.count_status(Indet, test_map),
};

let test_results = (~descriptions=[], map, term): option(test_results) => {
  switch (Core.Elaborator.uexp_elab(map, term) |> get_result) {
  | None
  | Some((_, [])) => None
  | Some((_, test_map)) => Some(mk_results(~descriptions, test_map))
  };
};
