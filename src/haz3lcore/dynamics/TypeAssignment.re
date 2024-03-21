open Util;
open OptUtil.Syntax;

let equal_typ_list = (l: list(Typ.t)): option(Typ.t) => {
  switch (l) {
  | [] => None
  | [ty, ..._] =>
    List.fold_left((acc, t) => {acc && Typ.eq(t, ty)}, true, l)
      ? Some(ty) : None
  };
};

let arrow_aux = (ty: Typ.t): Typ.t => {
  switch (ty) {
  | Unknown(Internal) => Arrow(Unknown(Internal), Unknown(Internal))
  | _ => ty
  };
};

let delta_ty = (id: MetaVar.t, m: Statics.Map.t): option(Typ.t) => {
  switch (Id.Map.find_opt(id, m)) {
  | Some(InfoExp({mode, ctx, _})) =>
    switch (mode) {
    | Syn
    | SynFun => Some(Unknown(Internal))
    | Ana(ana_ty) => Some(Typ.normalize(ctx, ana_ty))
    }
  | _ => None
  };
};

let ground = (ty: Typ.t): bool => {
  switch (ty) {
  | Bool
  | Int
  | Float
  | String
  | Prod([])
  | Arrow(Unknown(Internal), Unknown(Internal)) => true
  | _ => false
  };
};

let rec dhpat_extend_ctx = (dhpat: DHPat.t, ty: Typ.t, ctx: Ctx.t): Ctx.t => {
  switch (dhpat, ty) {
  | (Var(name), _) =>
    let entry = Ctx.VarEntry({name, id: Id.invalid, typ: ty});
    Ctx.extend(ctx, entry);
  | (Tuple(l1), Prod(l2)) =>
    if (List.length(l1) == List.length(l2)) {
      List.fold_left2(
        (acc, dhp, typ) => {dhpat_extend_ctx(dhp, typ, acc)},
        ctx,
        l1,
        l2,
      );
    } else {
      ctx;
    }
  | (Cons(dhp1, dhp2), List(typ)) =>
    ctx |> dhpat_extend_ctx(dhp1, typ) |> dhpat_extend_ctx(dhp2, ty)
  | (ListLit(typ1, l), List(typ2)) =>
    if (Typ.eq(typ1, typ2)) {
      List.fold_left(
        (acc, dhp) => {dhpat_extend_ctx(dhp, typ1, acc)},
        ctx,
        l,
      );
    } else {
      ctx;
    }
  | (Ap(Constructor(_, typ), dhp), _) =>
    let (ty1, ty2) = Typ.matched_arrow(ctx, typ);
    if (Typ.eq(ty2, ty)) {
      ctx |> dhpat_extend_ctx(dhp, ty1);
    } else {
      ctx;
    };
  | _ => ctx
  };
};

let rec typ_of_dhexp =
        (ctx: Ctx.t, m: Statics.Map.t, dh: DHExp.t): option(Typ.t) => {
  switch (dh) {
  | EmptyHole(id, _) => delta_ty(id, m)
  | NonEmptyHole(_, id, _, d) =>
    switch (typ_of_dhexp(ctx, m, d)) {
    | None => None
    | Some(_) => delta_ty(id, m)
    }
  | FreeVar(id, _, _) => delta_ty(id, m)
  | ExpandingKeyword(_)
  | InvalidText(_) => Some(Unknown(Internal))
  | InconsistentBranches(_, _, Case(d_scrut, d_rules, _)) =>
    let* ty' = typ_of_dhexp(ctx, m, d_scrut);
    let typ_cases =
      d_rules
      |> List.map((DHExp.Rule(dhp, de)) => {
           typ_of_dhexp(dhpat_extend_ctx(dhp, ty', ctx), m, de)
         })
      |> OptUtil.sequence;

    switch (typ_cases) {
    | None => None
    | Some(_) => Some(Typ.Unknown(Internal))
    };
  | Closure(env, d) =>
    let* l =
      env
      |> ClosureEnvironment.to_list
      |> List.map(((name, de)) => {
           let+ ty = typ_of_dhexp(ctx, m, de);
           Ctx.VarEntry({name, id: Id.invalid, typ: ty});
         })
      |> OptUtil.sequence;
    let ctx' =
      List.fold_left(
        (ctx, var_entry) => Ctx.extend(ctx, var_entry),
        ctx,
        l,
      );
    typ_of_dhexp(ctx', m, d);
  | Filter(_, d) => typ_of_dhexp(ctx, m, d)
  | BoundVar(name) =>
    let+ var = Ctx.lookup_var(ctx, name);
    var.typ;
  | Sequence(d1, d2) =>
    let* _ = typ_of_dhexp(ctx, m, d1);
    typ_of_dhexp(ctx, m, d2);
  | Let(dhp, de, db) =>
    let* ty1 = typ_of_dhexp(ctx, m, de);
    typ_of_dhexp(dhpat_extend_ctx(dhp, ty1, ctx), m, db);
  | FixF(name, ty1, d) =>
    let entry = Ctx.VarEntry({name, id: Id.invalid, typ: ty1});
    typ_of_dhexp(Ctx.extend(ctx, entry), m, d);
  | Fun(dhp, ty1, d, _) =>
    let+ ty2 = typ_of_dhexp(dhpat_extend_ctx(dhp, ty1, ctx), m, d);
    Typ.Arrow(ty1, ty2);
  | Ap(d1, d2) =>
    let* ty1 = typ_of_dhexp(ctx, m, d1);
    let* ty2 = typ_of_dhexp(ctx, m, d2);
    switch (arrow_aux(ty1)) {
    | Arrow(tyl, tyr) when Typ.eq(tyl, ty2) => Some(tyr)
    | _ => None
    };
  | ApBuiltin(_)
  | BuiltinFun(_) => None
  | Test(_, dtest) =>
    let* ty = typ_of_dhexp(ctx, m, dtest);
    Typ.eq(ty, Bool) ? Some(Typ.Prod([])) : None;
  | BoolLit(_) => Some(Bool)
  | IntLit(_) => Some(Int)
  | FloatLit(_) => Some(Float)
  | StringLit(_) => Some(String)
  | BinBoolOp(_, d1, d2) =>
    let* ty1 = typ_of_dhexp(ctx, m, d1);
    let* ty2 = typ_of_dhexp(ctx, m, d2);
    Typ.eq(ty1, Bool) && Typ.eq(ty2, Bool) ? Some(Typ.Bool) : None;
  | BinIntOp(op, d1, d2) =>
    let* ty1 = typ_of_dhexp(ctx, m, d1);
    let* ty2 = typ_of_dhexp(ctx, m, d2);
    if (Typ.eq(ty1, Int) && Typ.eq(ty2, Int)) {
      switch (op) {
      | Minus
      | Plus
      | Times
      | Power
      | Divide => Some(Typ.Int)
      | LessThan
      | LessThanOrEqual
      | GreaterThan
      | GreaterThanOrEqual
      | Equals
      | NotEquals => Some(Typ.Bool)
      };
    } else {
      None;
    };
  | BinFloatOp(op, d1, d2) =>
    let* ty1 = typ_of_dhexp(ctx, m, d1);
    let* ty2 = typ_of_dhexp(ctx, m, d2);
    if (Typ.eq(ty1, Float) && Typ.eq(ty2, Float)) {
      switch (op) {
      | Minus
      | Plus
      | Times
      | Power
      | Divide => Some(Typ.Float)
      | LessThan
      | LessThanOrEqual
      | GreaterThan
      | GreaterThanOrEqual
      | Equals
      | NotEquals => Some(Typ.Bool)
      };
    } else {
      None;
    };
  | BinStringOp(op, d1, d2) =>
    let* ty1 = typ_of_dhexp(ctx, m, d1);
    let* ty2 = typ_of_dhexp(ctx, m, d2);
    if (Typ.eq(ty1, String) && Typ.eq(ty2, String)) {
      switch (op) {
      | Concat => Some(Typ.String)
      | Equals => Some(Typ.Bool)
      };
    } else {
      None;
    };
  | ListLit(_, _, ty, _) => Some(List(ty))
  | Cons(d1, ListLit(_, _, _, [])) =>
    let* ty1 = typ_of_dhexp(ctx, m, d1);
    Some(Typ.List(ty1));
  | Cons(d1, d2) =>
    let* ty1 = typ_of_dhexp(ctx, m, d1);
    let* ty2 = typ_of_dhexp(ctx, m, d2);
    switch (ty2) {
    | List(ty3) when Typ.eq(ty3, ty1) => Some(ty2)
    | _ => None
    };
  | ListConcat(ListLit(_, _, _, []), d)
  | ListConcat(d, ListLit(_, _, _, [])) =>
    let* ty = typ_of_dhexp(ctx, m, d);
    Some(Typ.List(ty));
  | ListConcat(d1, d2) =>
    let* ty1 = typ_of_dhexp(ctx, m, d1);
    let* ty2 = typ_of_dhexp(ctx, m, d2);
    switch (ty1, ty2) {
    | (List(ty1), List(ty2)) when Typ.eq(ty1, ty2) => Some(Typ.List(ty1))
    | _ => None
    };
  | Tuple(dhs) =>
    let+ typ_list =
      dhs |> List.map(typ_of_dhexp(ctx, m)) |> OptUtil.sequence;
    Typ.Prod(typ_list);
  | Prj(dh, i) =>
    let* ty = typ_of_dhexp(ctx, m, dh);
    switch (ty) {
    | Prod(l) when List.length(l) > i => Some(List.nth(l, i))
    | _ => None
    };
  | Constructor(_, typ) => Some(typ)
  | ConsistentCase(Case(d_scrut, d_rules, _)) =>
    let* ty' = typ_of_dhexp(ctx, m, d_scrut);
    let* typ_cases: list(Typ.t) =
      d_rules
      |> List.map((DHExp.Rule(dhp, de)) => {
           typ_of_dhexp(dhpat_extend_ctx(dhp, ty', ctx), m, de)
         })
      |> OptUtil.sequence;
    equal_typ_list(typ_cases);
  | Cast(d, ty1, ty2) =>
    let* _ = Typ.join(~fix=true, ctx, ty1, ty2);
    let* tyd = typ_of_dhexp(ctx, m, d);
    Typ.eq(tyd, ty1) ? Some(ty2) : None;
  | FailedCast(d, ty1, ty2) =>
    if (ground(ty1) && ground(ty2) && !Typ.eq(ty1, ty2)) {
      let* tyd = typ_of_dhexp(ctx, m, d);
      Typ.eq(tyd, ty1) ? Some(ty2) : None;
    } else {
      None;
    }
  | InvalidOperation(d, _) => typ_of_dhexp(ctx, m, d)
  | IfThenElse(ConsistentIf, d_scrut, d1, d2) =>
    let* ty = typ_of_dhexp(ctx, m, d_scrut);
    if (Typ.eq(ty, Bool)) {
      let* ty1 = typ_of_dhexp(ctx, m, d1);
      let* ty2 = typ_of_dhexp(ctx, m, d2);
      equal_typ_list([ty1, ty2]);
    } else {
      None;
    };
  | IfThenElse(InconsistentIf, d_scrut, d1, d2) =>
    let* ty = typ_of_dhexp(ctx, m, d_scrut);
    if (Typ.eq(ty, Bool)) {
      let* _ = typ_of_dhexp(ctx, m, d1);
      let* _ = typ_of_dhexp(ctx, m, d2);
      Some(Typ.Unknown(Internal));
    } else {
      None;
    };
  };
};

let property_test = (uexp_typ: Typ.t, dhexp: DHExp.t, m: Statics.Map.t): bool => {
  let dhexp_typ = typ_of_dhexp(Builtins.ctx_init, m, dhexp);

  switch (dhexp_typ) {
  | None => false
  | Some(dh_typ) => Typ.eq(dh_typ, uexp_typ)
  };
};
