[@deriving sexp]
type t = (DHExp.t, HoleClosureInfo.t, Evaluator.result);

let get_dhexp = ((d, _, _): t) => d;

let final_dhexp_equals = (r1: Evaluator.result, r2: Evaluator.result): bool => {
  switch (r1, r2) {
  | (BoxedValue(d1), BoxedValue(d2))
  | (Indet(d1), Indet(d2)) => DHExp.fast_equals(d1, d2)
  | _ => false
  };
};

let fast_equals = ((_, hci1, r1): t, (_, hci2, r2): t): bool => {
  /* Check that HoleClosureInstances are equal */
  MetaVarMap.cardinal(hci1) == MetaVarMap.cardinal(hci2)
  && List.for_all2(
       /* Check that all holes are equal */
       ((u1, hcs1), (u2, hcs2)) =>
         u1 == u2
         && List.length(hcs1) == List.length(hcs2)
         && List.for_all2(
              /* Check that all hole closures are equal */
              (sigma1, sigma2) =>
                EvalEnv.id_of_evalenv(sigma1)
                == EvalEnv.id_of_evalenv(sigma2)
                && List.for_all2(
                     /* Check that variable mappings in evalenv are equal */
                     ((x1, r1), (x2, r2)) =>
                       x1 == x2 && final_dhexp_equals(r1, r2),
                     EvalEnv.result_map_of_evalenv(sigma1),
                     EvalEnv.result_map_of_evalenv(sigma2),
                   ),
              hcs1,
              hcs2,
            ),
       MetaVarMap.bindings(hci1),
       MetaVarMap.bindings(hci2),
     )
  /* Check that r1, r2 are equal */
  && final_dhexp_equals(r1, r2);
};
