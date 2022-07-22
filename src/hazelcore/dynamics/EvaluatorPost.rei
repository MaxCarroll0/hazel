/* EvalPostprocess.re: Postprocesses the evaluation result.

   This has two functions:
   - Match the evaluation result generated by evaluation with substitution.
     This means to continue evaluation within expressions for which evaluation
     has not reached (e.g., lambda expression bodies, unmatched case and let
     expression bodies), by looking up bound variables and assigning hole
     environments.
   - Number holes and generate a HoleClosureInfo.t that holds information
     about all unique hole closures in the result.

   The postprocessing steps are partially memoized by environments. (Only
   memoized among hole instances which share the same environment.)

   Algorithmically, this algorithm begins in the evaluated region of the
   evaluation result inside the "evaluation boundary" (pp_eval),
   and continues to the region outside the evaluation boundary (pp_uneval).
   */

/* Errors related to EvalPostprocess.postprocess

   Postprocessing invalid cases:
   Evaluation boundary is abbreviated as "EB". "In closure" and "outside closure"
   correspond to "outside the EB" and "inside the EB," respectively.

   The following errors are used to indicate an invalid case DURING postprocessing.
   - ClosureInsideClosure: an evaluated expression outside the EB
   - BoundVarOutsideClosure: an un-looked-up (unevaluated) variable inside the EB
   - UnevalOutsideClosure: non-variable unevaluated expression inside the EB
   - InvalidClosureBody: closures currently only make sense storing the following
     expression types:
     - Hole expressions
     - Lambda abstractions
     - Let/case with a pattern match failure

   The following errors are used to indicate an invalid case AFTER postprocessing.
   After postprocessing, closures around lambda abstractions, let expressions, and
   case expressions should be removed, and all hole expressions should be wrapped
   in a closure.
   - PostprocessedNoneHoleInClosure
   - PostprocessedHoleOutsideClosure
   */
[@deriving sexp]
type error =
  | ClosureInsideClosure
  | UnevalOutsideClosure
  | InvalidClosureBody
  | PostprocessedNonHoleInClosure
  | PostprocessedHoleOutsideClosure;

[@deriving sexp]
exception Exception(error);

/* Memoize postprocessed environments */
type t = EnvironmentIdMap.t(ClosureEnvironment.t);

/* Postprocess inside evaluation boundary. Environment should already
   be postprocessed */
let pp_uneval:
  (t, HoleClosureInfo_.t, ClosureEnvironment.t, DHExp.t) =>
  (t, HoleClosureInfo_.t, DHExp.t);

let pp_uneval_rules:
  (t, HoleClosureInfo_.t, ClosureEnvironment.t, list(DHExp.rule)) =>
  (t, HoleClosureInfo_.t, list(DHExp.rule));

/* Postprocess inside evaluation boundary */
let pp_eval:
  (t, HoleClosureInfo_.t, DHExp.t) => (t, HoleClosureInfo_.t, DHExp.t);

/* Recurse through environments, using memoized result if available. */
let pp_eval_env:
  (t, HoleClosureInfo_.t, ClosureEnvironment.t) =>
  (t, HoleClosureInfo_.t, ClosureEnvironment.t);

/* Tracking children of hole closures. A hole closure is a child of
   another hole closure if it exists in the hole environment of the parent.

   This is the second stage of postprocessing, separate from hole numbering
   and substitution, since memoization becomes much more convoluted if
   these two stages are combined.

   This works by simply iterating over all the (postprocessed)
   hole closure environments in the HoleClosureInfo_.t and looking for
   "child" holes.
   */
let track_children_of_hole:
  (HoleClosureInfo.t, HoleClosureParents.t_, DHExp.t) => HoleClosureInfo.t;

let track_children_of_hole_rules:
  (HoleClosureInfo.t, HoleClosureParents.t_, list(DHExp.rule)) =>
  HoleClosureInfo.t;

/* Driver for hole parent tracking; iterate through all hole closures
   in the HoleClosureInfo, and call `track_children_of_hole` on them. */
let track_children: HoleClosureInfo.t => HoleClosureInfo.t;

/* Postprocessing driver.

   Note: The top-level expression is wrapped in a non-empty hole, this
   is a clean way of noting holes that lie directly in the result.

   See also HoleClosureInfo.rei/HoleClosureInfo_.rei.
   */
let postprocess: DHExp.t => (HoleClosureInfo.t, DHExp.t);
