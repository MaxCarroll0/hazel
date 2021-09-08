open Sexplib.Std;

/* TODO: Hannah - change this from different lists that get converted to what
   type of thing you want to draw to just a single list of the things you want to draw
    (instead of doing that in the current method)  - each item has paths and a shape and the
    shape holds any other information you need to draw
    - make this its own branch/PR */
[@deriving sexp]
type t = {
  err_holes: list(CursorPath.steps),
  var_err_holes: list(CursorPath.steps),
  var_uses: list(CursorPath.steps),
  explanation_elems: list(CursorPath.steps),
  current_term: option(CursorPath.t),
};

let is_empty = (dpaths: t): bool =>
  ListUtil.is_empty(dpaths.err_holes)
  && ListUtil.is_empty(dpaths.var_err_holes)
  && ListUtil.is_empty(dpaths.var_uses)
  && ListUtil.is_empty(dpaths.explanation_elems)
  && dpaths.current_term == None;

let take_step = (step: int, dpaths: t): t => {
  let {err_holes, var_err_holes, current_term, var_uses, explanation_elems} = dpaths;
  let remove_step =
    fun
    | [step', ...steps] when step == step' => Some(steps)
    | _ => None;
  let err_holes = err_holes |> List.filter_map(remove_step);
  let var_err_holes = var_err_holes |> List.filter_map(remove_step);
  let var_uses = var_uses |> List.filter_map(remove_step);
  let explanation_elems = explanation_elems |> List.filter_map(remove_step);
  let current_term =
    Option.bind(current_term, ((steps, cursor)) =>
      remove_step(steps) |> Option.map(steps => (steps, cursor))
    );
  {err_holes, var_err_holes, var_uses, explanation_elems, current_term};
};

let current = (shape: TermShape.t, dpaths: t): list(UHDecorationShape.t) => {
  let is_current = steps =>
    switch (shape) {
    | SubBlock({hd_index, _}) => steps == [hd_index]
    | NTuple({comma_indices, _}) =>
      List.exists(n => steps == [n], comma_indices)
    | BinOp({op_index, _}) => steps == [op_index]
    | Operand
    | Case
    | Rule => steps == []
    };
  let err_holes =
    dpaths.err_holes
    |> List.find_opt(is_current)
    |> Option.map(_ => UHDecorationShape.ErrHole)
    |> Option.to_list;
  let var_err_holes =
    dpaths.var_err_holes
    |> List.find_opt(is_current)
    |> Option.map(_ => UHDecorationShape.VarErrHole)
    |> Option.to_list;
  let var_uses =
    dpaths.var_uses
    |> List.find_opt(is_current)
    |> Option.map(_ => UHDecorationShape.VarUse)
    |> Option.to_list;
  let explanation_elems =
    dpaths.explanation_elems
    |> List.find_opt(is_current)
    |> Option.map(_ => UHDecorationShape.ExplanationElems)
    |> Option.to_list;
  let current_term =
    switch (dpaths.current_term) {
    | Some((steps, _)) when is_current(steps) => [
        UHDecorationShape.CurrentTerm,
      ]
    | _ => []
    };
  List.concat([
    err_holes,
    var_err_holes,
    var_uses,
    explanation_elems,
    current_term,
  ]);
};
