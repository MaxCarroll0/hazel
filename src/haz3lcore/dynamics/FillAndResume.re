/*
     HOLE SUBSTITUTION:
     Ids are used as the metavars.
     TODO: ensure ids of holes and failed casts are persistented throughout evaluation. Primarily upon being passed into functions.
 */

let rec fill_any = (m: list(Id.t), s: DHExp.t, a: Any.t): Any.t => {
  switch (a) {
  | Exp(d) => Exp(fill_dhexp(m, s, d))
  | x => x
  };
}
and fill_dhexp =
    (m: list(Id.t), s: DHExp.t, {term, ids, copied} as d: DHExp.t): DHExp.t => {
  let rewrap = (d): DHExp.t => {term: d, ids, copied};
  let fill = fill_dhexp(m, s);
  let fill_all = List.map(fill);
  let fill_any = fill_any(m, s);
  let fill_env = fill_env(m, s);
  switch (term) {
  | EmptyHole => ids == m ? s : d
  | MultiHole(ds) =>
    ids == m ? s : MultiHole(List.map(fill_any, ds)) |> rewrap
  | DynamicErrorHole(d', err) =>
    ids == m ? s : DynamicErrorHole(fill(d'), err) |> rewrap // Probably not possible?
  | FailedCast(d', s1, s2) =>
    ids == m ? s : FailedCast(fill(d'), s1, s2) |> rewrap
  | Undefined
  | Invalid(_)
  | Deferral(_) // TODO: double check
  | Bool(_)
  | Int(_)
  | Float(_)
  | String(_)
  | Var(_)
  | BuiltinFun(_)
  | Constructor(_) => d // Probably not possible?
  | Cast(d', s1, s2) => Cast(fill(d'), s1, s2) |> rewrap
  | ListLit(ds) => ListLit(fill_all(ds)) |> rewrap
  | Cons(d1, d2) => Cons(fill(d1), fill(d2)) |> rewrap
  | ListConcat(d1, d2) => ListConcat(fill(d1), fill(d2)) |> rewrap
  | Tuple(ds) => Tuple(fill_all(ds)) |> rewrap
  | If(d1, d2, d3) => If(fill(d1), fill(d2), fill(d3)) |> rewrap
  | Seq(d1, d2) => Seq(fill(d1), fill(d2)) |> rewrap
  | Test(d') => Test(fill(d')) |> rewrap
  | Parens(d') => Parens(fill(d')) |> rewrap
  | UnOp(op, d') => UnOp(op, fill(d')) |> rewrap
  | BinOp(op, d1, d2) => BinOp(op, fill(d1), fill(d2)) |> rewrap
  | Fun(pat, d', Some(env), var) =>
    Fun(pat, fill(d'), Some(fill_env(env)), var) |> rewrap // TODO: Double check env logic
  | Fun(pat, d', None, var) => Fun(pat, fill(d'), None, var) |> rewrap
  | Ap(dir, d1, d2) => Ap(dir, fill(d1), fill(d2)) |> rewrap
  | DeferredAp(d', ds) => DeferredAp(fill(d'), fill_all(ds)) |> rewrap // TODO: Double check
  | FixF(pat, d', Some(env)) =>
    FixF(pat, fill(d'), Some(fill_env(env))) |> rewrap
  | FixF(pat, d', None) => FixF(pat, fill(d'), None) |> rewrap
  | Let(pat, d1, d2) => Let(pat, fill(d1), fill(d2)) |> rewrap
  | TypFun(tpat, d', var) => TypFun(tpat, fill(d'), var) |> rewrap
  | TypAp(d', ty) => TypAp(fill(d'), ty) |> rewrap
  | TyAlias(tpat, ty, d') => TyAlias(tpat, ty, fill(d')) |> rewrap
  | Match(d', cases) =>
    Match(fill(d'), cases |> List.map(((pat, d'')) => (pat, fill(d''))))
    |> rewrap
  | Filter(kind, d') => Filter(kind, fill(d')) |> rewrap
  | Closure(env, d') => Closure(fill_env(env), fill(d')) |> rewrap
  };
}
and fill_env =
    (m: list(Id.t), s: DHExp.t, env: ClosureEnvironment.t)
    : ClosureEnvironment.t => {
  ClosureEnvironment.map_keep_id(((_, d)) => fill_dhexp(m, s, d), env);
};
