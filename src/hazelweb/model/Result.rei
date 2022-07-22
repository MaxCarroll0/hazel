/* The result of a program evaluation. Includes the
   final `DHExp.t`, the tracked hole closure information,
   and the evaluation state. Generated by Program.get_result.

   Not to be confused with `Evaluator.result`, which is a
   wrapper around `DHExp.t` that indicates what kind of
   final result it is (BoxedValue/Indet).

   The `EvaluatorState.t` component includes `EvaluatorStats.t`, which
   may contain evaluation statistics (e.g., number of evaluation
   steps) and may serve as an initial state to resume evaluation
   from a previous evaluation result in fill-and-resume.
   */
[@deriving sexp]
type t = (DHExp.t, HoleInstanceInfo.t, EvaluatorResult.t, EvaluatorState.t);

let get_dhexp: t => DHExp.t;
let get_hole_closure_info: t => HoleInstanceInfo.t;
let get_eval_state: t => EvaluatorState.t;

/* See DHExp.fast_equals. Also checks that all environments
   in the HoleInstanceInfo.t are equal. */
let fast_equals: (t, t) => bool;
