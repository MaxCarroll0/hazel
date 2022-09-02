open OptUtil.Syntax;

let rec htyp_of_typ: Typ.t => HTyp.t =
  fun
  | Unknown(_) => Hole
  | Int => Int
  | Float => Float
  | Bool => Bool
  | List(t) => List(htyp_of_typ(t))
  | Arrow(t1, t2) => Arrow(htyp_of_typ(t1), htyp_of_typ(t2))
  | Prod(ts) => Prod(List.map(htyp_of_typ, ts));

let exp_self_htyp = (m, e) => htyp_of_typ(Statics.exp_self_typ(m, e));
let pat_self_htyp = (m, e) => htyp_of_typ(Statics.pat_self_typ(m, e));

let exp_htyp = (m, e) => htyp_of_typ(Statics.exp_typ(m, e));
let pat_htyp = (m, p) => htyp_of_typ(Statics.pat_typ(m, p));

let ctx_to_varctx = (ctx: Ctx.t): VarCtx.t =>
  List.map(((k, {typ, _}: Ctx.entry)) => (k, htyp_of_typ(typ)), ctx);

let int_op_of: Term.UExp.op_bin_int => DHExp.BinIntOp.t =
  fun
  | Plus => Plus
  | Minus => Minus
  | Times => Times
  | Divide => Divide
  | LessThan => LessThan
  | GreaterThan => GreaterThan
  | Equals => Equals;

let float_op_of: Term.UExp.op_bin_float => DHExp.BinFloatOp.t =
  fun
  | Plus => FPlus
  | Minus => FMinus
  | Times => FTimes
  | Divide => FDivide
  | LessThan => FLessThan
  | GreaterThan => FGreaterThan
  | Equals => FEquals;

let bool_op_of: Term.UExp.op_bin_bool => DHExp.BinBoolOp.t =
  fun
  | And => And
  | Or => Or;

let exp_binop_of: Term.UExp.op_bin => (HTyp.t, (_, _) => DHExp.t) =
  fun
  | Int(op) => (Int, ((e1, e2) => BinIntOp(int_op_of(op), e1, e2)))
  | Float(op) => (Float, ((e1, e2) => BinFloatOp(float_op_of(op), e1, e2)))
  | Bool(op) => (Bool, ((e1, e2) => BinBoolOp(bool_op_of(op), e1, e2)));

let rec dhexp_of_uexp = (m: Statics.map, uexp: Term.UExp.t): option(DHExp.t) => {
  /* NOTE: Left out delta for now */
  switch (Id.Map.find_opt(uexp.id, m)) {
  | Some(InfoExp({ctx, mode, self, _})) =>
    let err_status = Statics.error_status(mode, self);
    let maybe_reason: option(ErrStatus.HoleReason.t) =
      switch (err_status) {
      | NotInHole(_) => None
      | InHole(_) => Some(TypeInconsistent)
      };
    let u = uexp.id; /* NOTE: using term uids for hole ids */
    let gamma = ctx_to_varctx(ctx);
    let sigma = Environment.id_env(gamma);
    let wrap = (d: DHExp.t): option(DHExp.t) =>
      switch (maybe_reason) {
      | None => Some(d)
      | Some(reason) => Some(NonEmptyHole(reason, u, 0, sigma, d))
      };
    switch (uexp.term) {
    | Invalid(_) /* NOTE: treating invalid as a hole for now */
    | EmptyHole => Some(EmptyHole(u, 0, sigma))
    | MultiHole(_, []) =>
      // TODO: dhexp, eval for multiholes
      Some(EmptyHole(u, 0, sigma))
    | MultiHole(_, [e0, ...es]) =>
      // TODO: dhexp, eval for multiholes
      // placeholder logic: sequence
      let* ds =
        List.fold_left(
          (acc, e) => {
            let* acc = acc;
            let+ d = dhexp_of_uexp(m, e);
            DHExp.Sequence(d, acc);
          },
          dhexp_of_uexp(m, e0),
          es,
        );
      wrap(ds);
    | Triv => wrap(Triv)
    | Bool(b) => wrap(BoolLit(b))
    | Int(n) => wrap(IntLit(n))
    | Float(n) => wrap(FloatLit(n))
    | ListLit(_) =>
      //TODO: list literals. below is just placeholder
      wrap(ListNil(Hole))
    | Fun(p, body) =>
      let* dp = dhpat_of_upat(m, p);
      let* d1 = dhexp_of_uexp(m, body);
      let ty1 = pat_htyp(m, p);
      wrap(DHExp.Lam(dp, ty1, d1));
    | Tuple(_ids, es) =>
      //TODO(andrew): review below
      switch (List.rev(es)) {
      | [] => wrap(Triv)
      | [_] => failwith("ERROR: Tuple with one element")
      | [e0, ...es] =>
        let* ds =
          List.fold_left(
            (acc, e) => {
              let* acc = acc;
              let+ d = dhexp_of_uexp(m, e);
              DHExp.Pair(d, acc);
            },
            dhexp_of_uexp(m, e0),
            es,
          );
        wrap(ds);
      }
    | Cons(e1, e2) =>
      let* d1 = dhexp_of_uexp(m, e1);
      let* d2 = dhexp_of_uexp(m, e2);
      wrap(Cons(d1, d2));
    | UnOp(Int(Minus), e) =>
      let* d = dhexp_of_uexp(m, e);
      let ty = exp_self_htyp(m, e);
      let dc = DHExp.cast(d, ty, Int);
      wrap(BinIntOp(Minus, IntLit(0), dc));
    | BinOp(op, e1, e2) =>
      let (ty, cons) = exp_binop_of(op);
      let* d1 = dhexp_of_uexp(m, e1);
      let* d2 = dhexp_of_uexp(m, e2);
      let ty1 = exp_self_htyp(m, e1);
      let ty2 = exp_self_htyp(m, e2);
      let dc1 = DHExp.cast(d1, ty1, ty);
      let dc2 = DHExp.cast(d2, ty2, ty);
      wrap(cons(dc1, dc2));
    | Parens(e) => dhexp_of_uexp(m, e)
    | Seq(e1, e2) =>
      let* d1 = dhexp_of_uexp(m, e1);
      let* d2 = dhexp_of_uexp(m, e2);
      wrap(Sequence(d1, d2));
    | Test(test) =>
      let* dtest = dhexp_of_uexp(m, test);
      wrap(Ap(TestLit(u), dtest));
    | Var(name) =>
      switch (err_status) {
      | InHole(FreeVariable) => Some(FreeVar(u, 0, sigma, name))
      | _ => wrap(BoundVar(name))
      }
    | Let(
        {term: TypeAnn({term: Var(x), _}, {term: Arrow(_), _}), _} as p,
        {term: Fun(_), _} as def,
        body,
      ) =>
      /* NOTE: recursive case */
      let pat_typ = pat_self_htyp(m, p);
      let def_typ = exp_self_htyp(m, def);
      let* p = dhpat_of_upat(m, p);
      let* def = dhexp_of_uexp(m, def);
      let* body = dhexp_of_uexp(m, body);
      let cast_var = DHExp.cast(BoundVar(x), def_typ, pat_typ);
      let def_subst = Evaluator.subst_var(cast_var, x, def);
      wrap(Let(p, FixF(x, def_typ, def_subst), body));
    | Let(p, def, body) =>
      let* dp = dhpat_of_upat(m, p);
      let* ddef = dhexp_of_uexp(m, def);
      let* dbody = dhexp_of_uexp(m, body);
      wrap(Let(dp, ddef, dbody));
    | Ap(fn, arg) =>
      let* d_fn = dhexp_of_uexp(m, fn);
      let* d_arg = dhexp_of_uexp(m, arg);
      let ty_fn = exp_self_htyp(m, fn);
      let ty_arg = exp_self_htyp(m, arg);
      let* (ty_in, ty_out) = HTyp.matched_arrow(ty_fn);
      let c_fn = DHExp.cast(d_fn, ty_fn, HTyp.Arrow(ty_in, ty_out));
      let c_arg = DHExp.cast(d_arg, ty_arg, ty_in);
      wrap(Ap(c_fn, c_arg));
    | If(scrut, e1, e2) =>
      let* d_scrut = dhexp_of_uexp(m, scrut);
      let* d1 = dhexp_of_uexp(m, e1);
      let* d2 = dhexp_of_uexp(m, e2);
      let d_rules =
        DHExp.[Rule(BoolLit(true), d1), Rule(BoolLit(false), d2)];
      let d = DHExp.Case(d_scrut, d_rules, 0);
      switch (err_status) {
      | InHole(SynInconsistentBranches(_)) =>
        Some(DHExp.InconsistentBranches(u, 0, sigma, d))
      | _ => wrap(ConsistentCase(d))
      };
    | Match(_, scrut, rules) =>
      let* d_scrut = dhexp_of_uexp(m, scrut);
      let* d_rules =
        List.map(
          ((p, e)) => {
            let* d_p = dhpat_of_upat(m, p);
            let+ d_e = dhexp_of_uexp(m, e);
            DHExp.Rule(d_p, d_e);
          },
          rules,
        )
        |> OptUtil.sequence;
      let d = DHExp.Case(d_scrut, d_rules, 0);
      switch (err_status) {
      | InHole(SynInconsistentBranches(_)) =>
        Some(DHExp.InconsistentBranches(u, 0, sigma, d))
      | _ => wrap(ConsistentCase(d))
      };
    };
  | Some(InfoPat(_) | InfoTyp(_) | InfoRul(_) | Invalid(_))
  | None => None
  };
}
and dhpat_of_upat = (m: Statics.map, upat: Term.UPat.t): option(DHPat.t) => {
  switch (Id.Map.find_opt(upat.id, m)) {
  | Some(InfoPat({mode, self, _})) =>
    let err_status = Statics.error_status(mode, self);
    let maybe_reason: option(ErrStatus.HoleReason.t) =
      switch (err_status) {
      | NotInHole(_) => None
      | InHole(_) => Some(TypeInconsistent)
      };
    let u = upat.id; /* NOTE: using term uids for hole ids */
    let wrap = (d: DHPat.t): option(DHPat.t) =>
      switch (maybe_reason) {
      | None => Some(d)
      | Some(reason) => Some(NonEmptyHole(reason, u, 0, d))
      };
    switch (upat.term) {
    | Invalid(_) /* NOTE: treating invalid as a hole for now */
    | EmptyHole => Some(EmptyHole(u, 0))
    | MultiHole(_) =>
      // TODO: dhexp, eval for multiholes
      Some(EmptyHole(u, 0))
    | Wild => wrap(Wild)
    | Triv => wrap(Triv)
    | Bool(b) => wrap(BoolLit(b))
    | Int(n) => wrap(IntLit(n))
    | Float(n) => wrap(FloatLit(n))
    | ListNil => wrap(ListNil)
    | Cons(hd, tl) =>
      let* d_hd = dhpat_of_upat(m, hd);
      let* d_tl = dhpat_of_upat(m, tl);
      wrap(Cons(d_hd, d_tl));
    | Tuple(_ids, ps) =>
      //TODO(andrew): review below
      switch (List.rev(ps)) {
      | [] => wrap(Triv)
      | [_] => failwith("ERROR: Tuple with one element")
      | [p0, ...ps] =>
        let* ds =
          List.fold_left(
            (acc, p) => {
              let* acc = acc;
              let+ d = dhpat_of_upat(m, p);
              DHPat.Pair(d, acc);
            },
            dhpat_of_upat(m, p0),
            ps,
          );
        wrap(ds);
      }
    | Var(name) => Some(Var(name))
    | Parens(p) => dhpat_of_upat(m, p)
    | TypeAnn(p, _ty) =>
      let* dp = dhpat_of_upat(m, p);
      wrap(dp);
    };
  | Some(InfoExp(_) | InfoTyp(_) | InfoRul(_) | Invalid(_))
  | None => None
  };
};

let uexp_elab =
    (m: Statics.map, uexp: Term.UExp.t): Elaborator_Exp.ElaborationResult.t =>
  switch (dhexp_of_uexp(m, uexp)) {
  | None => DoesNotElaborate
  | Some(d) => Elaborates(d, HTyp.Hole, Delta.empty) //TODO: get type from ci
  };
