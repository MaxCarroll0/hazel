/** Types with holes */
include HTypCore;

type join =
  | GLB
  | LUB;

let rec equivalent = (ty: t, ty': t, k: Kind.t, ctx: TyCtx.t): bool =>
  switch (ty, ty') {
  | (TyVar(i1, _), ty2)
  | (ty2, TyVar(i1, _)) =>
    switch (ctx |> TyCtx.var_kind(i1)) {
    | Some(k1) => ctx |> Kind.equivalent(k1, Singleton(k, ty2))
    | None => failwith(__LOC__ ++ ": unbound type variable")
    }
  | (TyVarHole(_, u, _), ty2)
  | (ty2, TyVarHole(_, u, _)) =>
    // TODO: (johnson) figure out what goes here
    switch (ctx |> TyCtx.hole_kind(u)) {
    | Some(k1) => ctx |> Kind.equivalent(k1, Singleton(k, ty2))
    | None => failwith(__LOC__ ++ ": unbound type hole")
    }
  | (Hole(u), ty2)
  | (ty2, Hole(u)) =>
    // TODO: (johnson) figure out what goes here
    switch (ctx |> TyCtx.hole_kind(u)) {
    | Some(k1) => ctx |> Kind.equivalent(k1, Singleton(k, ty2))
    | None => failwith(__LOC__ ++ ": unbound type hole")
    }
  | (Int | Float | Bool, _) => ty == ty'
  | (Arrow(ty1, ty2), Arrow(ty1', ty2'))
  | (Sum(ty1, ty2), Sum(ty1', ty2')) =>
    // TODO: (johnson) what goes here?
    let k1 = (??);
    let k2 = (??);
    ctx |> equivalent(ty1, ty1', k1) && ctx |> equivalent(ty2, ty2', k2);
  | (Arrow(_, _), _) => false
  | (Sum(_, _), _) => false
  | (Prod(tys1), Prod(tys2)) =>
    let check_component = (ty1, ty2, k3) => ctx |> equivalent(ty1, ty2, k3);
    ListUtil.for_all3_opt(check_component, tys1, tys2, ks)
    |> Option.value(~default=false);
  | (Prod(_), _) => false
  | (List(ty), List(ty')) =>
    // TODO: (johnson) what goes here?
    let k1 = (??);
    ctx |> equivalent(ty, ty', k1);
  | (List(_), _) => false
  };

/** Type consistency

Types are consistent if they are equivalent modulo holes.
*/
let rec consistent = (ty: t, ty': t, ctx: TyCtx.t) =>
  switch (ty, ty') {
  /* holes are consistent with anything */
  | (TyVarHole(_), _)
  | (_, TyVarHole(_))
  | (Hole(_), _)
  | (_, Hole(_)) => true
  /* type variables are consistent with equivalent types */
  | (TyVar(_), _)
  | (_, TyVar(_)) => ctx |> equivalent(ty, ty', k)
  | (Int | Float | Bool, _) => ty == ty'
  | (Arrow(ty1, ty2), Arrow(ty1', ty2'))
  | (Sum(ty1, ty2), Sum(ty1', ty2')) =>
    ctx |> consistent(ty1, ty1') && ctx |> consistent(ty2, ty2')
  | (Arrow(_, _), _) => false
  | (Sum(_, _), _) => false
  | (Prod(tys1), Prod(tys2)) =>
    let check_component = (ty1, ty2) => ctx |> consistent(ty1, ty2);
    ListUtil.for_all2_opt(check_component, tys1, tys2)
    |> Option.value(~default=false);
  | (Prod(_), _) => false
  | (List(ty), List(ty')) => ctx |> consistent(ty, ty')
  | (List(_), _) => false
  };

let inconsistent = (ty1: t, ty2: t, ctx: TyCtx.t) =>
  !(ctx |> consistent(ty1, ty2));

let rec consistent_all = (types: list(t), ctx: TyCtx.t): bool =>
  switch (types) {
  | [] => true
  | [hd, ...tl] =>
    !List.exists(x => ctx |> inconsistent(hd, x), tl)
    || consistent_all(tl, ctx)
  };

let rec join = (j: join, ty1: t, ty2: t, ctx: TyCtx.t): option(t) => {
  switch (ty1, ty2) {
  | (TyVarHole(_, u, _), TyVarHole(_, u', _)) =>
    Some(Hole(MetaVar.join(u, u')))
  | (_, Hole(_))
  | (_, TyVarHole(_)) =>
    switch (j) {
    | GLB => Some(Hole(0))
    | LUB => Some(ty1)
    }
  | (Hole(_), _)
  | (TyVarHole(_), _) =>
    switch (j) {
    | GLB => Some(Hole(0))
    | LUB => Some(ty2)
    }
  | (TyVar(i, _), _) =>
    open OptUtil.Syntax;
    let* (_, k) = ctx |> TyCtx.var_binding(i);
    switch (k) {
    | Singleton(_, ty) => ctx |> join(j, ty, ty2)
    | KHole => ctx |> join(j, Hole(0), ty2)
    | Type => failwith("impossible for bounded type variables (currently) 1")
    };
  | (_, TyVar(i, _)) =>
    open OptUtil.Syntax;
    let* (_, k) = ctx |> TyCtx.var_binding(i);
    switch (k) {
    | KindCore.Singleton(_, ty) => ctx |> join(j, ty1, ty)
    | KHole => ctx |> join(j, ty1, Hole(0))
    | Type => failwith("impossible for bounded type variables (currently) 2")
    };
  | (Int, Int) => Some(ty1)
  | (Int, _) => None
  | (Float, Float) => Some(ty1)
  | (Float, _) => None
  | (Bool, Bool) => Some(ty1)
  | (Bool, _) => None
  | (Arrow(ty1, ty2), Arrow(ty1', ty2')) =>
    open OptUtil.Syntax;
    let* ty1 = ctx |> join(j, ty1, ty1');
    let+ ty2 = ctx |> join(j, ty2, ty2');
    Arrow(ty1, ty2);
  | (Arrow(_), _) => None
  | (Sum(ty1, ty2), Sum(ty1', ty2')) =>
    open OptUtil.Syntax;
    let* ty1 = ctx |> join(j, ty1, ty1');
    let+ ty2 = ctx |> join(j, ty2, ty2');
    Sum(ty1, ty2);
  | (Sum(_), _) => None
  | (Prod(tys1), Prod(tys2)) =>
    ListUtil.map2_opt((ty1, ty2) => ctx |> join(j, ty1, ty2), tys1, tys2)
    |> Option.map(OptUtil.sequence)
    |> Option.join
    |> Option.map(joined_types => Prod(joined_types))
  | (Prod(_), _) => None
  | (List(ty), List(ty')) =>
    open OptUtil.Syntax;
    let+ ty = ctx |> join(j, ty, ty');
    List(ty);
  | (List(_), _) => None
  };
};

let join_all = (j: join, types: list(t), ctx: TyCtx.t): option(t) => {
  switch (types) {
  | [] => None
  | [hd] => Some(hd)
  | [hd, ...tl] =>
    if (!consistent_all(types, ctx)) {
      None;
    } else {
      List.fold_left(
        (common_opt, ty) =>
          switch (common_opt) {
          | None => None
          | Some(common_ty) => ctx |> join(j, common_ty, ty)
          },
        Some(hd),
        tl,
      );
    }
  };
};

let new_Hole = (u_gen: MetaVarGen.t): (t, MetaVarGen.t) => {
  let (u, u_gen) = MetaVarGen.next(u_gen);
  (Hole(u), u_gen);
};
