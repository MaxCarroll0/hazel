/**
 * Whether dp contains the variable x outside of a hole.
 */
let rec binds_var = (m: Statics.Map.t, x: Var.t, dp: DHPat.t): bool =>
  switch (Statics.get_pat_error_at(m, Pat.rep_id(dp))) {
  | Some(_) => false
  | None =>
    switch (dp |> Pat.term_of) {
    | EmptyHole
    | MultiHole(_)
    | Wild
    | Invalid(_)
    | Int(_)
    | Float(_)
    | Bool(_)
    | String(_)
    | Constructor(_) => false
    | Cast(y, _, _)
    | Parens(y) => binds_var(m, x, y)
    | Var(y) => Var.eq(x, y)
    | Tuple(dps) => dps |> List.exists(binds_var(m, x))
    | Cons(dp1, dp2) => binds_var(m, x, dp1) || binds_var(m, x, dp2)
    | ListLit(d_list) =>
      let new_list = List.map(binds_var(m, x), d_list);
      List.fold_left((||), false, new_list);
    | Ap(_, _) => false
    }
  };

/* closed substitution [d1/x]d2 */
let rec subst_var = (m, d1: DHExp.t, x: Var.t, d2: DHExp.t): DHExp.t => {
  let (term, rewrap) = DHExp.unwrap(d2);
  switch (term) {
  | Var(y) =>
    if (Var.eq(x, y)) {
      d1;
    } else {
      d2;
    }
  | Invalid(_) => d2
  | Undefined => d2
  | Seq(d3, d4) =>
    let d3 = subst_var(m, d1, x, d3);
    let d4 = subst_var(m, d1, x, d4);
    Seq(d3, d4) |> rewrap;
  | Filter(filter, dbody) =>
    let dbody = subst_var(m, d1, x, dbody);
    let filter = subst_var_filter(m, d1, x, filter);
    Filter(filter, dbody) |> rewrap;
  | Let(dp, d3, d4) =>
    let d3 = subst_var(m, d1, x, d3);
    let d4 =
      if (binds_var(m, x, dp)) {
        d4;
      } else {
        subst_var(m, d1, x, d4);
      };
    Let(dp, d3, d4) |> rewrap;
  | FixF(y, d3, env) =>
    let env' = Option.map(subst_var_env(m, d1, x), env);
    let d3 =
      if (binds_var(m, x, y)) {
        d3;
      } else {
        subst_var(m, d1, x, d3);
      };
    FixF(y, d3, env') |> rewrap;
  | Fun(dp, d3, ty, s) =>
    if (binds_var(m, x, dp)) {
      Fun(dp, d3, ty, s) |> rewrap;
    } else {
      let d3 = subst_var(m, d1, x, d3);
      Fun(dp, d3, ty, s) |> rewrap;
    }
  | TypFun(tpat, d3, s) =>
    TypFun(tpat, subst_var(m, d1, x, d3), s) |> rewrap
  | Closure(env, d3) =>
    /* Closure shouldn't appear during substitution (which
       only is called from elaboration currently) */
    let env' = subst_var_env(m, d1, x, env);
    let d3' = subst_var(m, d1, x, d3);
    Closure(env', d3') |> rewrap;
  | Ap(dir, d3, d4) =>
    let d3 = subst_var(m, d1, x, d3);
    let d4 = subst_var(m, d1, x, d4);
    Ap(dir, d3, d4) |> rewrap;
  | BuiltinFun(_) => d2
  | Test(d3) => Test(subst_var(m, d1, x, d3)) |> rewrap
  | Bool(_)
  | Int(_)
  | Float(_)
  | String(_)
  | Constructor(_) => d2
  | ListLit(ds) => ListLit(List.map(subst_var(m, d1, x), ds)) |> rewrap
  | Cons(d3, d4) =>
    let d3 = subst_var(m, d1, x, d3);
    let d4 = subst_var(m, d1, x, d4);
    Cons(d3, d4) |> rewrap;
  | ListConcat(d3, d4) =>
    let d3 = subst_var(m, d1, x, d3);
    let d4 = subst_var(m, d1, x, d4);
    ListConcat(d3, d4) |> rewrap;
  | Tuple(ds) => Tuple(List.map(subst_var(m, d1, x), ds)) |> rewrap
  | UnOp(op, d3) =>
    let d3 = subst_var(m, d1, x, d3);
    UnOp(op, d3) |> rewrap;
  | BinOp(op, d3, d4) =>
    let d3 = subst_var(m, d1, x, d3);
    let d4 = subst_var(m, d1, x, d4);
    BinOp(op, d3, d4) |> rewrap;
  | Match(ds, rules) =>
    let ds = subst_var(m, d1, x, ds);
    let rules =
      List.map(
        ((p, v)) =>
          if (binds_var(m, x, p)) {
            (p, v);
          } else {
            (p, subst_var(m, d1, x, v));
          },
        rules,
      );
    Match(ds, rules) |> rewrap;
  | EmptyHole => EmptyHole |> rewrap
  // TODO: handle multihole
  | MultiHole(_d2) => d2 //MultiHole(List.map(subst_var(m, d1, x), ds)) |> rewrap
  | Cast(d, ty1, ty2) =>
    let d' = subst_var(m, d1, x, d);
    Cast(d', ty1, ty2) |> rewrap;
  | FailedCast(d, ty1, ty2) =>
    let d' = subst_var(m, d1, x, d);
    FailedCast(d', ty1, ty2) |> rewrap;
  | DynamicErrorHole(d, err) =>
    let d' = subst_var(m, d1, x, d);
    DynamicErrorHole(d', err) |> rewrap;
  | If(d4, d5, d6) =>
    let d4' = subst_var(m, d1, x, d4);
    let d5' = subst_var(m, d1, x, d5);
    let d6' = subst_var(m, d1, x, d6);
    If(d4', d5', d6') |> rewrap;
  | TyAlias(tp, ut, d4) =>
    let d4' = subst_var(m, d1, x, d4);
    TyAlias(tp, ut, d4') |> rewrap;
  | Parens(d4) =>
    let d4' = subst_var(m, d1, x, d4);
    Parens(d4') |> rewrap;
  | Deferral(_) => d2
  | DeferredAp(d3, d4s) =>
    let d3 = subst_var(m, d1, x, d3);
    let d4s = List.map(subst_var(m, d1, x), d4s);
    DeferredAp(d3, d4s) |> rewrap;
  | TypAp(d3, ut) =>
    let d3 = subst_var(m, d1, x, d3);
    TypAp(d3, ut) |> rewrap;
  };
}

and subst_var_env =
    (m, d1: DHExp.t, x: Var.t, env: ClosureEnvironment.t)
    : ClosureEnvironment.t => {
  let id = env |> ClosureEnvironment.id_of;
  let map =
    env
    |> ClosureEnvironment.map_of
    |> Environment.foldo(
         ((x', d': DHExp.t), map) => {
           let d' =
             switch (DHExp.term_of(d')) {
             /* Substitute each previously substituted binding into the
              * fixpoint. */
             | FixF(_) =>
               map
               |> Environment.foldo(
                    ((x'', d''), d) => subst_var(m, d'', x'', d),
                    d',
                  )
             | _ => d'
             };

           /* Substitute. */
           let d' = subst_var(m, d1, x, d');
           Environment.extend(map, (x', d'));
         },
         Environment.empty,
       );

  ClosureEnvironment.wrap(id, map);
}

and subst_var_filter =
    (m, d1: DHExp.t, x: Var.t, flt: TermBase.StepperFilterKind.t)
    : TermBase.StepperFilterKind.t => {
  flt |> TermBase.StepperFilterKind.map(subst_var(m, d1, x));
};

let subst = (m, env: Environment.t, d: DHExp.t): DHExp.t =>
  env
  |> Environment.foldo(
       (xd: (Var.t, DHExp.t), d2) => {
         let (x, d1) = xd;
         subst_var(m, d1, x, d2);
       },
       d,
     );
