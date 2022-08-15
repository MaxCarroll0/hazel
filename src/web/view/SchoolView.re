open Virtual_dom.Vdom;
open Node;
open Util.Web;

let join_tile = (id): Core.Tile.t => {
  id,
  label: [";"],
  mold: Core.Mold.mk_bin(10, Exp, []),
  shards: [0],
  children: [],
};

let splice_editors = (editors: list(Model.editor)): Core.Segment.t => {
  editors
  |> List.map((ed: Model.editor) => {
       let (zipper, _) = Core.Perform.drop_it_like_its_hot(ed.zipper, 6000);
       Core.Zipper.unselect_and_zip(zipper);
     })
  |> (
    xs =>
      Util.ListUtil.interleave(
        xs,
        List.init(List.length(editors) - 1, i =>
          [Core.Piece.Tile(join_tile(i + 1000000))]
        ) //TODO(andrew): id_gen hack
      )
  )
  |> List.flatten;
};

let spliced_statics = (editors: list(Model.editor)) => {
  let term = editors |> splice_editors |> Core.MakeTerm.go;
  let (_, _, info_map) = term |> Core.Statics.mk_map;
  (term, info_map);
};

let coverage_summary_str = (~total, ~found): string => {
  TestView.result_summary_str(
    ~n=total,
    ~p=found,
    ~q=0,
    ~n_str="bug",
    ~ns_str="bugs",
    ~p_str="found",
    ~q_str="",
    ~r_str="unrevealed",
  );
};

let coverage_text = (~total, ~found): Node.t =>
  div(
    [clss(["test-text"])],
    [
      TestView.percent_view(total, found),
      div([], [text(":")]),
      text(coverage_summary_str(~total, ~found)),
    ],
  );

let coverage_bar = (~inject as _, instances) =>
  div(
    [clss(["test-bar"])],
    List.map(
      ((status, _)) =>
        div([clss(["segment", TestStatus.to_string(status)])], []),
      instances,
    ),
  );

let coverage_summary = (~inject, instances) => {
  let total = List.length(instances);
  let found =
    List.length(
      List.filter(((x: TestStatus.t, _)) => x == Pass, instances),
    );
  let status_class = total == found ? "Pass" : "Fail";
  div(
    [clss(["test-summary", "instructional-msg", status_class])],
    [coverage_text(~total, ~found), coverage_bar(~inject, instances)],
  );
};

let coverage_report_view = (i, (status, instance), ~inject, ~font_metrics) =>
  div(
    [clss(["test-report"]), Attr.on_click(TestView.jump_to_test(~inject))],
    [
      div(
        [clss(["test-id", "Test" ++ TestStatus.to_string(status)])],
        /* NOTE: prints lexical index, not unique id */
        [text(string_of_int(i + 1))],
      ),
      switch (instance) {
      | Some(instance) =>
        TestView.test_instance_view(~font_metrics, instance)
      | None => div([], [])
      },
    ],
  );

let passing_test_ids = test_map =>
  test_map
  |> List.filter(((_id, reports)) =>
       List.for_all(((_, status)) => status == TestStatus.Pass, reports)
     )
  |> List.split
  |> fst;

let failing_test_ids = test_map =>
  test_map
  |> List.filter(((_id, reports)) =>
       List.for_all(((_, status)) => status == TestStatus.Fail, reports)
     )
  |> List.split
  |> fst;

let get_test_map = (editors: list(Model.editor)) => {
  let (reference_term, reference_map) = spliced_statics(editors);
  let result_reference =
    Interface.test_results(reference_map, reference_term);
  switch (result_reference) {
  | None => []
  | Some(test_results) => test_results.test_map
  };
};

let get_first_common =
    (reference_passing, wrong): (TestStatus.t, option('a)) => {
  let wrong_test_map = wrong |> get_test_map;
  let wrong_failing = wrong_test_map |> failing_test_ids;
  //print_endline("wrong failing:"); //print_endline(show_blah(wrong_failing));
  let common =
    List.filter(x => List.mem(x, reference_passing), wrong_failing);
  let instance: option(list('a)) =
    switch (common) {
    | [] => None
    | [x, ..._] => List.assoc_opt(x, wrong_test_map)
    };
  switch (instance) {
  | Some([instance, ..._]) => (TestStatus.Pass, Some(instance))
  | _ => (TestStatus.Fail, None)
  };
};

let coverage_view = (~font_metrics, ~inject, reference, wrongs) => {
  ///TODO: clean up this dogshit function
  let reference_passing = reference |> get_test_map |> passing_test_ids;
  //print_endline("reference passing:"); //print_endline(show_blah(reference_passing));
  let instances = List.map(get_first_common(reference_passing), wrongs);
  div(
    [clss(["panel", "test-panel"])],
    [
      TestView.view_of_main_title_bar("Test Coverage:"),
      div(
        [clss(["panel-body", "test-reports"])],
        List.mapi(coverage_report_view(~inject, ~font_metrics), instances),
      ),
      coverage_summary(~inject, instances),
    ],
  );
};
