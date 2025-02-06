/* STATICS.re

   This module determines the statics semantics of a program.
   It makes use of the following modules:

   INFO.re: Defines the Info.t type which is used to represent the
   static STATUS of a term. This STATUS can be either OK or ERROR,
   and is determined by reconcilling two sources of typing information,
   the MODE and the SELF.

   MODE.re: Defines the Mode.t type which is used to represent the
   typing expectations imposed by a term's ancestors.

   SELF.re: Define the Self.t type which is used to represent the
   type information derivable from the term itself.

   The point of STATICS.re itself is to derive a map between each
   term's unique id and that term's static INFO. The below functions
   are intended mostly as infrastructure: The point is to define a
   traversal through the syntax tree which, for each term, passes
   down the MODE, passes up the SELF, calculates the INFO, and adds
   it to the map.

   The architectural intention here is that most type-manipulation
   logic is defined in INFO, MODE, and SELF, and the STATICS module
   itself is dedicated to the piping necessary to (A) introduce and
   (B) propagate the necessary information through the syntax tree.

    */

module Info = Info;

module Map = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Id.Map.t(Info.t);

  let error_ids = (info_map: t): list(Id.t) =>
    Id.Map.fold(
      (id, info, acc) =>
        /* Second clause is to eliminate non-representative ids,
         * which will not be found in the measurements map */
        Info.is_error(info) && id == Info.id_of(info) ? [id, ...acc] : acc,
      info_map,
      [],
    );
};

let map_m = (f, xs, m: Map.t) =>
  List.fold_left(
    ((xs, m), x) => f(x, m) |> (((x, m)) => (xs @ [x], m)),
    ([], m),
    xs,
  );

let add_info = (ids: list(Id.t), info: Info.t, m: Map.t): Map.t =>
  ids |> List.fold_left((m, id) => Id.Map.add(id, info, m), m);

let rec is_arrow_like = (t: Typ.t) => {
  switch (t.term) {
  | Unknown(_) => true
  | Arrow(_) => true
  | Forall(_, t) => is_arrow_like(t)
  | _ => false
  };
};

let is_recursive = (ctx, p, def, syn: TypSlice.t) => {
  switch (Pat.get_num_of_vars(p), Exp.get_num_of_functions(def)) {
  | (Some(num_vars), Some(num_fns))
      when num_vars != 0 && num_vars == num_fns =>
    let norm = Typ.normalize(ctx, syn |> TypSlice.typ_of);
    switch (norm |> Typ.term_of) {
    | Prod(syns) when List.length(syns) == num_vars =>
      syns |> List.for_all(is_arrow_like)
    | _ when is_arrow_like(norm) => num_vars == 1
    | _ => false
    };
  | _ => false
  };
};

let typ_exp_binop_bin_int: Operators.op_bin_int => Typ.term =
  fun
  | (Plus | Minus | Times | Power | Divide) as _op => Int
  | (
      LessThan | GreaterThan | LessThanOrEqual | GreaterThanOrEqual | Equals |
      NotEquals
    ) as _op =>
    Bool;

let typ_exp_binop_bin_float: Operators.op_bin_float => Typ.term =
  fun
  | (Plus | Minus | Times | Power | Divide) as _op => Float
  | (
      LessThan | GreaterThan | LessThanOrEqual | GreaterThanOrEqual | Equals |
      NotEquals
    ) as _op =>
    Bool;

let typ_exp_binop_bin_string: Operators.op_bin_string => Typ.term =
  fun
  | Concat => String
  | Equals => Bool;

let typ_exp_binop: Operators.op_bin => (Typ.term, Typ.term, Typ.term) =
  fun
  | Bool(And | Or) => (Bool, Bool, Bool)
  | Int(op) => (Int, Int, typ_exp_binop_bin_int(op))
  | Float(op) => (Float, Float, typ_exp_binop_bin_float(op))
  | String(op) => (String, String, typ_exp_binop_bin_string(op));

let typ_exp_unop: Operators.op_un => (Typ.term, Typ.term) =
  fun
  | Meta(Unquote) => (Var("$Meta"), Unknown(Internal))
  | Bool(Not) => (Bool, Bool)
  | Int(Minus) => (Int, Int);

let rec any_to_info_map =
        (~ctx: Ctx.t, ~ancestors, any: Any.t, m: Map.t): (CoCtx.t, Map.t) =>
  switch (any) {
  | Exp(e) =>
    let ({co_ctx, _}: Info.exp, m) =
      uexp_to_info_map(~ctx, ~ancestors, e, m);
    (co_ctx, m);
  | Pat(p) =>
    let m =
      upat_to_info_map(
        ~is_synswitch=false,
        ~co_ctx=CoCtx.empty,
        ~ancestors,
        ~ctx,
        p,
        m,
      )
      |> snd;
    (CoCtx.empty, m);
  | TPat(tp) => (
      CoCtx.empty,
      utpat_to_info_map(~ctx, ~ancestors, tp, m) |> snd,
    )
  | Typ(ty) => (
      CoCtx.empty,
      utyp_to_info_map(~ctx, ~ancestors, ty |> TypSlice.t_of_typ_t, m) |> snd,
    )
  | TypSlice(ty) => (
      CoCtx.empty,
      utyp_to_info_map(~ctx, ~ancestors, ty, m) |> snd,
    )
  | Rul(_)
  | Any () => (CoCtx.empty, m)
  }
and multi = (~ctx, ~ancestors, m, tms) =>
  List.fold_left(
    ((co_ctxs, m), any) => {
      let (co_ctx, m) = any_to_info_map(~ctx, ~ancestors, any, m);
      (co_ctxs @ [co_ctx], m);
    },
    ([], m),
    tms,
  )
and uexp_to_info_map =
    (
      ~ctx: Ctx.t,
      ~mode=Mode.Syn,
      ~is_in_filter=false,
      ~ancestors,
      {ids, copied: _, term} as uexp: Exp.t,
      m: Map.t,
    )
    : (Info.exp, Map.t) => {
  /* Maybe switch mode to syn */
  let mode =
    switch (mode) {
    | Ana(ty) when TypSlice.is_synswitch(ty) => Mode.Syn
    | _ => mode
    };
  let add' = (~self, ~co_ctx, m) => {
    let info =
      Info.derived_exp(~uexp, ~ctx, ~mode, ~ancestors, ~self, ~co_ctx);
    (info, add_info(ids, InfoExp(info), m));
  };
  let add = (~self, ~co_ctx, m) => add'(~self=Common(self), ~co_ctx, m);
  let ancestors = [Exp.rep_id(uexp)] @ ancestors;
  let uexp_to_info_map =
      (
        ~ctx,
        ~mode=Mode.Syn,
        ~is_in_filter=is_in_filter,
        ~ancestors=ancestors,
        uexp: Exp.t,
        m: Map.t,
      ) => {
    uexp_to_info_map(~ctx, ~mode, ~is_in_filter, ~ancestors, uexp, m);
  };
  let go' = uexp_to_info_map(~ancestors);
  let go = go'(~ctx);
  let map_m_go = m =>
    List.fold_left2(
      ((es, m), mode, e) =>
        go(~mode, e, m) |> (((e, m)) => (es @ [e], m)),
      ([], m),
    );
  let go_pat = upat_to_info_map(~ctx, ~ancestors);
  let go_typ = utyp_to_info_map(~ctx, ~ancestors);
  let atomic = self => add(~self, ~co_ctx=CoCtx.empty, m);
  switch (term) {
  | Closure(_, e) =>
    // TODO: implement closure type checking properly - see how dynamic type assignment does it
    let (e, m) = go(~mode, e, m);
    add(~self=Just(e.ty), ~co_ctx=e.co_ctx, m);
  | MultiHole(tms) =>
    let (co_ctxs, m) = multi(~ctx, ~ancestors, m, tms);
    add(~self=IsMulti, ~co_ctx=CoCtx.union(co_ctxs), m);
  | Cast(e, _, t2)
  | FailedCast(e, _, t2) =>
    let (t, m) = go_typ(t2, ~expects=Info.TypeExpected, m);
    let (e, m) = go'(~mode=Ana(t.term), ~ctx=t.ctx, e, m);
    add(~self=Just(t.term), ~co_ctx=e.co_ctx, m);
  | Invalid(token) => atomic(BadToken(token))
  | EmptyHole => atomic(Self.hole)
  | Deferral(position) =>
    add'(~self=IsDeferral(position), ~co_ctx=CoCtx.empty, m)
  | Undefined =>
    atomic(Just(`Typ(Unknown(Hole(EmptyHole))) |> TypSlice.temp))
  | Bool(_) => atomic(Bool |> Self.of_base(ids))
  | Int(_) => atomic(Int |> Self.of_base(ids))
  | Float(_) => atomic(Float |> Self.of_base(ids))
  | String(_) => atomic(String |> Self.of_base(ids))
  | ListLit(es) =>
    let e_ids = List.map(Exp.rep_id, es);
    let modes = Mode.of_list_lit(ids, ctx, List.length(es), mode);
    let (es, m) = map_m_go(m, modes, es);
    let tys = List.map(Info.exp_ty, es);
    add(
      ~self=
        Self.of_list_lit(
          ~empty=`Typ(Unknown(Internal)) |> TypSlice.temp,
          ids,
          ctx,
          tys,
          e_ids,
        ),
      ~co_ctx=CoCtx.union(List.map(Info.exp_co_ctx, es)),
      m,
    );
  | Cons(hd, tl) =>
    let (hd, m) = go(~mode=Mode.of_cons_hd(ids, ctx, mode), hd, m);
    let (tl, m) = go(~mode=Mode.of_cons_tl(ids, ctx, mode, hd.ty), tl, m);
    add(
      ~self=hd.ty |> Self.of_list_cons(ids),
      ~co_ctx=CoCtx.union([hd.co_ctx, tl.co_ctx]),
      m,
    );
  | ListConcat(e1, e2) =>
    let mode = Mode.of_list_concat(ids, ctx, mode);
    let c_ids = List.map(Exp.rep_id, [e1, e2]);
    let (e1, m) = go(~mode, e1, m);
    let (e2, m) = go(~mode, e2, m);
    add(
      ~self=Self.of_list_concat(ids, ctx, [e1.ty, e2.ty], c_ids),
      ~co_ctx=CoCtx.union([e1.co_ctx, e2.co_ctx]),
      m,
    );
  | Var(name) =>
    add'(
      ~self=Self.of_exp_var(ids, ctx, name),
      ~co_ctx=CoCtx.singleton(name, Exp.rep_id(uexp), Mode.ty_of(mode)),
      m,
    )
  | DynamicErrorHole(e, _)
  | Parens(e) =>
    let (e, m) = go(~mode=Mode.of_parens(ids, mode), e, m);
    add(~self=e.ty |> Self.of_parens(ids), ~co_ctx=e.co_ctx, m);
  | UnOp(Meta(Unquote), e) when is_in_filter =>
    let e: Exp.t = {
      ids: e.ids,
      copied: false,
      term:
        switch (e.term) {
        | Var("e") => Constructor("$e", Unknown(Internal) |> Typ.temp)
        | Var("v") => Constructor("$v", Unknown(Internal) |> Typ.temp)
        | _ => e.term
        },
    };
    let ty_in: Typ.term = Var("$Meta");
    let (e, m) = go(~mode=ty_in |> Mode.of_op(ids), e, m);
    add(~self=Self.hole, ~co_ctx=e.co_ctx, m);
  | UnOp(op, e) =>
    let (ty_in, ty_out) = typ_exp_unop(op);
    let (e, m) = go(~mode=ty_in |> Mode.of_op(ids), e, m);
    add(~self=Self.of_op(ids, ty_out), ~co_ctx=e.co_ctx, m);
  | BinOp(op, e1, e2) =>
    let (ty1, ty2, ty_out) = typ_exp_binop(op);
    let (e1, m) = go(~mode=ty1 |> Mode.of_op(ids), e1, m);
    let (e2, m) = go(~mode=ty2 |> Mode.of_op(ids), e2, m);
    add(
      ~self=ty_out |> Self.of_op(ids),
      ~co_ctx=CoCtx.union([e1.co_ctx, e2.co_ctx]),
      m,
    );
  | BuiltinFun(string) =>
    add'(
      ~self=Self.of_exp_var(ids, Builtins.ctx_init, string),
      ~co_ctx=CoCtx.empty,
      m,
    )
  | Tuple(es) =>
    let modes = Mode.of_prod(ids, ctx, mode, List.length(es));
    let (es, m) = map_m_go(m, modes, es);
    add(
      ~self=List.map(Info.exp_ty, es) |> Self.of_prod(ids),
      ~co_ctx=CoCtx.union(List.map(Info.exp_co_ctx, es)),
      m,
    );
  | Test(e) =>
    let (e, m) = go(~mode=Bool |> Mode.of_op(ids), e, m);
    add(~self=Prod([]) |> Self.of_op(ids), ~co_ctx=e.co_ctx, m);
  | Filter(Filter({pat: cond, _}), body) =>
    // TODO: Slicing
    let (cond, m) = go(~mode=Syn, cond, m, ~is_in_filter=true);
    let (body, m) = go(~mode, body, m);
    add(
      ~self=body.ty |> Self.of_filter(ids),
      ~co_ctx=CoCtx.union([cond.co_ctx, body.co_ctx]),
      m,
    );
  | Filter(Residue(_), body) =>
    // TODO: Check slicing
    let (body, m) = go(~mode, body, m);
    add(
      ~self=body.ty |> Self.of_filter(ids),
      ~co_ctx=CoCtx.union([body.co_ctx]),
      m,
    );
  | Seq(e1, e2) =>
    let (e1, m) = go(~mode=Syn, e1, m);
    let (e2, m) = go(~mode, e2, m);
    add(
      ~self=e2.ty |> Self.of_seq(ids),
      ~co_ctx=CoCtx.union([e1.co_ctx, e2.co_ctx]),
      m,
    );
  | Constructor(ctr, ty) => atomic(Self.of_ctr(ids, ctx, ctr, ty))
  | Ap(_, fn, arg) =>
    let fn_mode = Mode.of_ap(ctx, mode, Exp.ctr_name(fn));
    let (fn, m) = go(~mode=fn_mode, fn, m);
    let (ty_in, ty_out) = TypSlice.matched_arrow(ctx, fn.ty);
    let (arg, m) = go(~mode=ty_in |> Mode.of_ap_arg(ids), arg, m);
    let self: Self.t = Self.of_ap(ids, ctx, arg.term.ids, ty_in, ty_out); // TODO: Re-add incremental ids used in arrow type ty_in -> ty_out
    add(~self, ~co_ctx=CoCtx.union([fn.co_ctx, arg.co_ctx]), m);
  | TypAp(fn, utyp) =>
    let typfn_mode = Mode.typap_mode;
    let (fn, m) = go(~mode=typfn_mode, fn, m);
    let (_, m) =
      utyp_to_info_map(
        ~ctx,
        ~ancestors,
        utyp |> TypSlice.t_of_typ_t_sliced,
        m,
      );
    let self = fn.ty |> Self.of_typap(ids, ctx, utyp);
    add(~self, ~co_ctx=fn.co_ctx, m);
  | DeferredAp(fn, args) =>
    // TODO: Slicing
    let fn_mode = Mode.of_ap(ctx, mode, Exp.ctr_name(fn));
    let (fn, m) = go(~mode=fn_mode, fn, m);
    let (ty_in, ty_out) = TypSlice.matched_arrow(ctx, fn.ty);
    let num_args = List.length(args);
    let ty_ins = TypSlice.matched_args(ctx, num_args, ty_in);
    let self: Self.exp = Self.of_deferred_ap(args, ty_ins, ty_out);
    let modes = Mode.of_deferred_ap_args(num_args, ty_ins);
    let (args, m) = map_m_go(m, modes, args);
    let arg_co_ctx = CoCtx.union(List.map(Info.exp_co_ctx, args));
    add'(~self, ~co_ctx=CoCtx.union([fn.co_ctx, arg_co_ctx]), m);
  | Fun(p, e, typ, _) =>
    let (mode_pat, mode_body) = Mode.of_arrow(ids, ctx, mode, typ);
    let (p', _) =
      go_pat(~is_synswitch=false, ~co_ctx=CoCtx.empty, ~mode=mode_pat, p, m);
    let (e, m) = go'(~ctx=p'.ctx, ~mode=mode_body, e, m);
    /* add co_ctx to pattern */
    let (p, m) =
      go_pat(~is_synswitch=false, ~co_ctx=e.co_ctx, ~mode=mode_pat, p, m);
    let is_exhaustive = p |> Info.pat_constraint |> Incon.is_exhaustive;
    let self = Self.of_fun(ids, is_exhaustive, p.ty, e.ty);
    add'(~self, ~co_ctx=CoCtx.mk(ctx, p.ctx, e.co_ctx), m);
  | TypFun({term: Var(name), _} as utpat, body, _)
      when !Ctx.shadows_typ(ctx, name) =>
    let mode_body = Mode.of_forall(ctx, Some(name), mode);
    let m = utpat_to_info_map(~ctx, ~ancestors, utpat, m) |> snd;
    let ctx_body =
      Ctx.extend_tvar(ctx, {name, id: TPat.rep_id(utpat), kind: Abstract});
    let (body, m) = go'(~ctx=ctx_body, ~mode=mode_body, body, m);
    add(~self=body.ty |> Self.of_typfun(ids, utpat), ~co_ctx=body.co_ctx, m);
  | TypFun(utpat, body, _) =>
    let mode_body = Mode.of_forall(ctx, None, mode);
    let m = utpat_to_info_map(~ctx, ~ancestors, utpat, m) |> snd;
    let (body, m) = go(~mode=mode_body, body, m);
    add(~self=body.ty |> Self.of_typfun(ids, utpat), ~co_ctx=body.co_ctx, m);
  | Let(p, def, body) =>
    let (p_syn, _) =
      go_pat(~is_synswitch=true, ~co_ctx=CoCtx.empty, ~mode=Syn, p, m);
    let (def, p_ana_ctx, m, ty_p_ana) =
      if (!is_recursive(ctx, p, def, p_syn.ty)) {
        let (def, m) = go(~mode=Ana(p_syn.ty), def, m);
        let ty_p_ana = def.ty;
        let (p_ana', _) =
          go_pat(
            ~is_synswitch=false,
            ~co_ctx=CoCtx.empty,
            ~mode=Ana(ty_p_ana),
            p,
            m,
          );
        (def, p_ana'.ctx, m, ty_p_ana);
      } else {
        let (def_base, _) =
          go'(~ctx=p_syn.ctx, ~mode=Ana(p_syn.ty), def, m);
        let ty_p_ana = def_base.ty;
        /* Analyze pattern to incorporate def type into ctx */
        let (p_ana', _) =
          go_pat(
            ~is_synswitch=false,
            ~co_ctx=CoCtx.empty,
            ~mode=Ana(ty_p_ana),
            p,
            m,
          );
        let def_ctx = p_ana'.ctx;
        let (def_base2, _) = go'(~ctx=def_ctx, ~mode=Ana(p_syn.ty), def, m);
        let ana_ty_fn = ((ty_fn1, ty_fn2), ty_p) => {
          TypSlice.typ_of(ty_p)
          |> Typ.term_of == Unknown(SynSwitch)
          && !TypSlice.eq(ty_fn1, ty_fn2)
            ? ty_fn1 : ty_p;
        };

        let ana =
          switch (
            (
              def_base.ty
              |> TypSlice.term_of
              |> TypSlice.typslc_typ_term_of_term,
              def_base2.ty
              |> TypSlice.term_of
              |> TypSlice.typslc_typ_term_of_term,
            ),
            p_syn.ty |> TypSlice.term_of |> TypSlice.typslc_typ_term_of_term,
          ) {
          | (
              (Slice(Prod(ty_fns1)), Slice(Prod(ty_fns2))),
              Slice(Prod(ty_ps)),
            ) =>
            let tys =
              List.map2(ana_ty_fn, List.combine(ty_fns1, ty_fns2), ty_ps);
            `SliceIncr((Slice(Prod(tys)), TypSlice.empty_slice_incr))
            |> TypSlice.temp;
          | (
              (Typ(Prod(ty_fns1)), Typ(Prod(ty_fns2))),
              Typ(Prod(ty_ps)),
            ) =>
            let tys =
              List.map2(
                ana_ty_fn,
                List.combine(
                  List.map(TypSlice.t_of_typ_t, ty_fns1),
                  List.map(TypSlice.t_of_typ_t, ty_fns2),
                ),
                List.map(TypSlice.t_of_typ_t, ty_ps),
              );
            `SliceIncr((Slice(Prod(tys)), TypSlice.empty_slice_incr))
            |> TypSlice.temp;

          | ((_, _), _) => ana_ty_fn((def_base.ty, def_base2.ty), p_syn.ty)
          };
        let (def, m) = go'(~ctx=def_ctx, ~mode=Ana(ana), def, m);
        (def, def_ctx, m, ty_p_ana);
      };
    let (body, m) = go'(~ctx=p_ana_ctx, ~mode, body, m);
    /* add co_ctx to pattern */
    let (p_ana, m) =
      go_pat(
        ~is_synswitch=false,
        ~co_ctx=body.co_ctx,
        ~mode=Ana(ty_p_ana),
        p,
        m,
      );
    let is_exhaustive = p_ana |> Info.pat_constraint |> Incon.is_exhaustive;
    let self = Self.of_let(ids, is_exhaustive, body.ty);
    add'(
      ~self,
      ~co_ctx=
        CoCtx.union([def.co_ctx, CoCtx.mk(ctx, p_ana.ctx, body.co_ctx)]),
      m,
    );
  | FixF(p, e, _) =>
    let (p', _) =
      go_pat(~is_synswitch=false, ~co_ctx=CoCtx.empty, ~mode, p, m);
    let (e', m) = go'(~ctx=p'.ctx, ~mode=Ana(p'.ty), e, m);
    let (p'', m) =
      go_pat(~is_synswitch=false, ~co_ctx=e'.co_ctx, ~mode, p, m);
    add(
      ~self=p'.ty |> Self.of_fix(ids),
      ~co_ctx=CoCtx.union([CoCtx.mk(ctx, p''.ctx, e'.co_ctx)]),
      m,
    );
  | If(e0, e1, e2) =>
    let branch_ids = List.map(Exp.rep_id, [e1, e2]);
    let (cond, m) = go(~mode=Ana(`Typ(Bool) |> TypSlice.temp), e0, m);
    let (cons, m) = go(~mode, e1, m);
    let (alt, m) = go(~mode, e2, m);
    add(
      ~self=Self.of_match(ids, ctx, [cons.ty, alt.ty], branch_ids),
      ~co_ctx=CoCtx.union([cond.co_ctx, cons.co_ctx, alt.co_ctx]),
      m,
    );
  | Match(scrut, rules) =>
    let (scrut, m) = go(~mode=Syn, scrut, m);
    let (ps, es) = List.split(rules);
    let branch_ids = List.map(Exp.rep_id, es);
    let (ps', _) =
      map_m(
        go_pat(
          ~is_synswitch=false,
          ~co_ctx=CoCtx.empty,
          ~mode=Mode.Ana(scrut.ty),
        ),
        ps,
        m,
      );
    let p_ctxs = List.map(Info.pat_ctx, ps');
    let (es, m) =
      List.fold_left2(
        ((es, m), e, ctx) =>
          go'(~ctx, ~mode, e, m) |> (((e, m)) => (es @ [e], m)),
        ([], m),
        es,
        p_ctxs,
      );
    let e_tys = List.map(Info.exp_ty, es);
    let e_co_ctxs =
      List.map2(CoCtx.mk(ctx), p_ctxs, List.map(Info.exp_co_ctx, es));
    let unwrapped_self: Self.exp =
      Common(Self.of_match(ids, ctx, e_tys, branch_ids));
    let constraint_ty =
      switch (scrut.ty |> TypSlice.typ_of |> Typ.term_of) {
      | Unknown(_) =>
        map_m(go_pat(~is_synswitch=false, ~co_ctx=CoCtx.empty), ps, m)
        |> fst
        |> List.map(Info.pat_ty)
        |> TypSlice.join_all(
             ~empty=`Typ(Unknown(Internal)) |> TypSlice.temp,
             ctx,
           )
      | _ => Some(scrut.ty)
      };
    let (self, m) =
      switch (constraint_ty) {
      | Some(constraint_ty) =>
        let pats_to_info_map = (ps: list(Pat.t), m) => {
          /* Add co-ctxs to patterns */
          List.fold_left(
            ((m, acc_constraint), (p, co_ctx)) => {
              let p_constraint =
                go_pat(
                  ~is_synswitch=false,
                  ~co_ctx,
                  ~mode=Mode.Ana(constraint_ty),
                  p,
                  m,
                )
                |> fst
                |> Info.pat_constraint;
              let (p, m) =
                go_pat(
                  ~is_synswitch=false,
                  ~co_ctx,
                  ~mode=Mode.Ana(scrut.ty),
                  p,
                  m,
                );
              let is_redundant =
                Incon.is_redundant(p_constraint, acc_constraint);
              let self = is_redundant ? Self.Redundant(p.self) : p.self;
              let info =
                Info.derived_pat(
                  ~upat=p.term,
                  ~ctx=p.ctx,
                  ~co_ctx=p.co_ctx,
                  ~mode=p.mode,
                  ~ancestors=p.ancestors,
                  ~prev_synswitch=None,
                  ~self,
                  // Mark patterns as redundant at the top level
                  // because redundancy doesn't make sense in a smaller context
                  ~constraint_=p_constraint,
                );
              (
                // Override the info for the single upat
                add_info(p.term.ids, InfoPat(info), m),
                is_redundant
                  ? acc_constraint  // Redundant patterns are ignored
                  : Constraint.Or(p_constraint, acc_constraint),
              );
            },
            (m, Constraint.Falsity),
            List.combine(ps, e_co_ctxs),
          );
        };
        let (m, final_constraint) = pats_to_info_map(ps, m);
        let is_exhaustive = Incon.is_exhaustive(final_constraint);
        let self =
          is_exhaustive ? unwrapped_self : InexhaustiveMatch(unwrapped_self);
        (self, m);
      | None =>
        /* Add co-ctxs to patterns */
        let (_, m) =
          map_m(
            ((p, co_ctx)) =>
              go_pat(
                ~is_synswitch=false,
                ~co_ctx,
                ~mode=Mode.Ana(scrut.ty),
                p,
              ),
            List.combine(ps, e_co_ctxs),
            m,
          );
        (unwrapped_self, m);
      };
    add'(~self, ~co_ctx=CoCtx.union([scrut.co_ctx] @ e_co_ctxs), m);
  | TyAlias(typat, utyp, body) =>
    let m = utpat_to_info_map(~ctx, ~ancestors, typat, m) |> snd;
    switch (typat.term) {
    | Var(name) when !Ctx.shadows_typ(ctx, name) =>
      /* Currently we disallow all type shadowing */
      /* NOTE(andrew): Currently, TypSlice.to_typ returns Unknown(TypeHole)
         for any type variable reference not in its ctx. So any free variables
         in the definition would be obliterated. But we need to check for free
         variables to decide whether to make a recursive type or not. So we
         tentatively add an abtract type to the ctx, representing the
         speculative rec parameter. */
      let (ty_def, ctx_def, ctx_body) = {
        let utyp_slice = utyp |> TypSlice.t_of_typ_t_sliced;
        switch (utyp.term) {
        | Sum(_) when List.mem(name, Typ.free_vars(utyp)) =>
          /* NOTE: When debugging type system issues it may be beneficial to
             use a different name than the alias for the recursive parameter */
          //let ty_rec = TypSlice.Rec("α", TypSlice.subst(Var("α"), name, ty_pre));
          let ty_rec =
            `SliceIncr((
              Slice(
                Rec((Var(name): TPat.term) |> IdTagged.fresh, utyp_slice),
              ),
              TypSlice.slice_of_ids(ids),
            ))
            |> TypSlice.temp;
          let ctx_def =
            Ctx.extend_alias(ctx, name, TPat.rep_id(typat), ty_rec);
          (ty_rec, ctx_def, ctx_def);
        | _ => (
            utyp_slice,
            ctx,
            Ctx.extend_alias(ctx, name, TPat.rep_id(typat), utyp_slice),
          )
        /* NOTE(yuchen): Below is an alternative implementation that attempts to
           add a rec whenever type alias is present. It may cause trouble to the
           runtime, so precede with caution. */
        // TypSlice.lookup_surface(ty_pre)
        //   ? {
        //     let ty_rec = TypSlice.Rec({item: ty_pre, name});
        //     let ctx_def = Ctx.add_alias(ctx, name, utpat_id(typat), ty_rec);
        //     (ty_rec, ctx_def, ctx_def);
        //   }
        //   : {
        //     let ty = Term.TypSlice.to_typ(ctx, utyp);
        //     (ty, ctx, Ctx.add_alias(ctx, name, utpat_id(typat), ty));
        //   };
        };
      };
      let ctx_body =
        switch (TypSlice.get_sum_constructors(ctx, ty_def)) {
        | Some(sm) =>
          Ctx.add_ctrs(
            [TPat.rep_id(typat)],
            ctx_body,
            name,
            Typ.rep_id(utyp),
            sm,
          )
        | None => ctx_body
        };
      let ({co_ctx, ty: ty_body, _}: Info.exp, m) =
        go'(~ctx=ctx_body, ~mode, body, m);
      /* Make sure types don't escape their scope */
      let ty_escape = TypSlice.subst(ty_def, typat, ty_body);
      let m =
        utyp_to_info_map(
          ~ctx=ctx_def,
          ~ancestors,
          utyp |> TypSlice.t_of_typ_t_sliced,
          m,
        )
        |> snd;
      // TODO: Correct type slicing
      add(
        ~self=Just(ty_escape |> TypSlice.(wrap_incr(slice_of_ids(ids)))),
        ~co_ctx,
        m,
      );
    | Var(_)
    | Invalid(_)
    | EmptyHole
    | MultiHole(_) =>
      let ({co_ctx, ty: ty_body, _}: Info.exp, m) =
        go'(~ctx, ~mode, body, m);
      let m =
        utyp_to_info_map(~ctx, ~ancestors, utyp |> TypSlice.t_of_typ_t, m)
        |> snd;
      add(~self=Just(ty_body), ~co_ctx, m);
    };
  };
}
and upat_to_info_map =
    (
      ~is_synswitch,
      ~ctx,
      ~co_ctx,
      ~ancestors: Info.ancestors,
      ~mode: Mode.t=Mode.Syn,
      {ids, term, _} as upat: Pat.t,
      m: Map.t,
    )
    : (Info.pat, Map.t) => {
  let add = (~self, ~ctx, ~constraint_, m) => {
    let prev_synswitch =
      switch (Id.Map.find_opt(Pat.rep_id(upat), m)) {
      | Some(Info.InfoPat({mode: Syn | SynFun, ty, _})) => Some(ty)
      | Some(Info.InfoPat({mode: Ana(_), prev_synswitch, _})) => prev_synswitch
      | Some(_)
      | None => None
      };
    let info =
      Info.derived_pat(
        ~prev_synswitch,
        ~upat,
        ~ctx,
        ~co_ctx,
        ~mode,
        ~ancestors,
        ~self=Common(self),
        ~constraint_,
      );
    (info, add_info(ids, InfoPat(info), m));
  };
  let atomic = (self, constraint_) => add(~self, ~ctx, ~constraint_, m);
  let ancestors = [Pat.rep_id(upat)] @ ancestors;
  let go = upat_to_info_map(~is_synswitch, ~ancestors, ~co_ctx);
  let unknown =
    // TODO: Check how slices interact with SynSwitch
    `Typ(Unknown(is_synswitch ? SynSwitch : Internal)) |> TypSlice.temp;
  let ctx_fold = (ctx: Ctx.t, m) =>
    List.fold_left2(
      ((ctx, tys, cons, m), e, mode) =>
        go(~ctx, ~mode, e, m)
        |> (
          ((info, m)) => (
            info.ctx,
            tys @ [info.ty],
            cons @ [info.constraint_],
            m,
          )
        ),
      (ctx, [], [], m),
    );
  let hole = self => atomic(self, Constraint.Hole);
  switch (term) {
  | MultiHole(tms) =>
    let (_, m) = multi(~ctx, ~ancestors, m, tms);
    add(~self=IsMulti, ~ctx, ~constraint_=Constraint.Hole, m);
  | Invalid(token) => hole(BadToken(token))
  | EmptyHole => hole(Just(unknown))
  | Int(int) => atomic(Int |> Self.of_base(ids), Constraint.Int(int))
  | Float(float) =>
    atomic(Float |> Self.of_base(ids), Constraint.Float(float))
  | Tuple([]) => atomic(Prod([]) |> Self.of_base(ids), Constraint.Truth)
  | Bool(bool) =>
    atomic(
      Bool |> Self.of_base(ids),
      bool
        ? Constraint.InjL(Constraint.Truth)
        : Constraint.InjR(Constraint.Truth),
    )
  | String(string) =>
    atomic(String |> Self.of_base(ids), Constraint.String(string))
  | ListLit(ps) =>
    let ids = List.map(Pat.rep_id, ps);
    let modes = Mode.of_list_lit(ids, ctx, List.length(ps), mode);
    let (ctx, tys, cons, m) = ctx_fold(ctx, m, ps, modes);
    let rec cons_fold_list = cs =>
      switch (cs) {
      | [] => Constraint.InjL(Constraint.Truth) // Left = nil, Right = cons
      | [hd, ...tl] =>
        Constraint.InjR(Constraint.Pair(hd, cons_fold_list(tl)))
      };
    add(
      ~self=Self.of_list_lit(~empty=unknown, ids, ctx, tys, ids),
      ~ctx,
      ~constraint_=cons_fold_list(cons),
      m,
    );
  | Cons(hd, tl) =>
    let (hd, m) = go(~ctx, ~mode=Mode.of_cons_hd(ids, ctx, mode), hd, m);
    let (tl, m) =
      go(~ctx=hd.ctx, ~mode=Mode.of_cons_tl(ids, ctx, mode, hd.ty), tl, m);
    add(
      ~self=hd.ty |> Self.of_list_cons(ids),
      ~ctx=tl.ctx,
      ~constraint_=
        Constraint.InjR(Constraint.Pair(hd.constraint_, tl.constraint_)),
      m,
    );
  | Wild => atomic(Just(unknown), Constraint.Truth)
  | Var(name) =>
    /* NOTE: The self type assigned to pattern variables (Unknown)
       may be SynSwitch, but SynSwitch is never added to the context;
       Unknown(Internal) is used in this case */
    let ctx_typ =
      Info.fixed_typ_pat(
        ctx,
        mode,
        Common(Just(`Typ(Unknown(Internal)) |> TypSlice.temp)),
      );
    let entry = Ctx.VarEntry({name, id: Pat.rep_id(upat), typ: ctx_typ});
    add(
      ~self=Just(unknown),
      ~ctx=Ctx.extend(ctx, entry),
      ~constraint_=Constraint.Truth,
      m,
    );
  | Tuple(ps) =>
    let modes = Mode.of_prod(ids, ctx, mode, List.length(ps));
    let (ctx, tys, cons, m) = ctx_fold(ctx, m, ps, modes);
    let rec cons_fold_tuple = cs =>
      switch (cs) {
      | [] => Constraint.Truth
      | [elt] => elt
      | [hd, ...tl] => Constraint.Pair(hd, cons_fold_tuple(tl))
      };
    add(
      ~self=tys |> Self.of_prod(ids),
      ~ctx,
      ~constraint_=cons_fold_tuple(cons),
      m,
    );
  | Parens(p) =>
    let (p, m) = go(~ctx, ~mode=Mode.of_parens(ids, mode), p, m);
    add(
      ~self=p.ty |> Self.of_parens(ids),
      ~ctx=p.ctx,
      ~constraint_=p.constraint_,
      m,
    );
  | Constructor(ctr, ty) =>
    let self = Self.of_ctr(ids, ctx, ctr, ty);
    atomic(self, Constraint.of_ctr(ctx, mode, ctr, self));
  | Ap(fn, arg) =>
    let ctr = Pat.ctr_name(fn);
    let fn_mode = Mode.of_ap(ctx, mode, ctr);
    let (fn, m) = go(~ctx, ~mode=fn_mode, fn, m);
    let (ty_in, ty_out) = TypSlice.matched_arrow(ctx, fn.ty);
    let (arg, m) = go(~ctx, ~mode=Ana(ty_in), arg, m);
    add(
      ~self=ty_out |> Self.of_ap_ok(ids),
      ~ctx=arg.ctx,
      ~constraint_=
        Constraint.of_ap(ctx, mode, ctr, arg.constraint_, Some(ty_out)),
      m,
    );
  | Cast(p, ann, _) =>
    let (ann, m) = utyp_to_info_map(~ctx, ~ancestors, ann, m);
    let (p, m) = go(~ctx, ~mode=Mode.of_ann(ids, ann.term), p, m);
    add(
      ~self=ann.term |> Self.of_annot(ids),
      ~ctx=p.ctx,
      ~constraint_=p.constraint_,
      m,
    );
  };
}
and utyp_to_info_map =
    (
      ~ctx,
      ~expects=Info.TypeExpected,
      ~ancestors,
      {ids, term, _} as utyp: TypSlice.t,
      m: Map.t,
    )
    : (Info.typ, Map.t) => {
  let add = m => {
    let info = Info.derived_typ(~utyp, ~ctx, ~ancestors, ~expects);
    (info, add_info(ids, InfoTyp(info), m));
  };
  let ancestors = [TypSlice.rep_id(utyp)] @ ancestors;
  let go' = utyp_to_info_map(~ctx, ~ancestors);
  let go = go'(~expects=TypeExpected);
  // Ensure f_typ and f_slc coincide: TODO: remove redundancy.
  let f_typ = (term: Typ.term) =>
    switch (term) {
    | Unknown(Hole(MultiHole(tms))) =>
      let (_, m) = multi(~ctx, ~ancestors, m, tms);
      add(m);
    | Unknown(_)
    | Int
    | Float
    | Bool
    | String => add(m)
    | Var(_) =>
      /* Names are resolved in Info.status_typ */
      add(m)
    | List(t)
    | Parens(t) => add(go(t |> TypSlice.t_of_typ_t, m) |> snd)
    | Arrow(t1, t2) =>
      let m = go(t1 |> TypSlice.t_of_typ_t, m) |> snd;
      let m = go(t2 |> TypSlice.t_of_typ_t, m) |> snd;
      add(m);
    | Prod(ts) =>
      let m = map_m(go, List.map(TypSlice.t_of_typ_t, ts), m) |> snd;
      add(m);
    | Ap(t1, t2) =>
      let t1_mode: Info.typ_expects =
        switch (expects) {
        | VariantExpected(m, sum_ty) =>
          ConstructorExpected(
            m,
            `SliceIncr((
              Slice(Arrow(t2 |> TypSlice.t_of_typ_t, sum_ty)),
              TypSlice.empty_slice_incr,
            ))
            |> TypSlice.temp,
          )
        | _ =>
          ConstructorExpected(
            Unique,
            `Typ(Arrow(t2, Unknown(Internal) |> Typ.temp)) |> TypSlice.temp,
          )
        };
      let m = go'(~expects=t1_mode, t1 |> TypSlice.t_of_typ_t, m) |> snd;
      let m = go'(~expects=TypeExpected, t2 |> TypSlice.t_of_typ_t, m) |> snd;
      add(m);
    | Sum(variants) =>
      let (m, _) =
        List.fold_left(
          variant_to_info_map(~ctx, ~ancestors, ~ty_sum=utyp),
          (m, []),
          ConstructorMap.map_vals(TypSlice.t_of_typ_t, variants),
        );
      add(m);
    | Forall({term: Var(name), _} as utpat, tbody) =>
      let body_ctx =
        Ctx.extend_tvar(
          ctx,
          {name, id: TPat.rep_id(utpat), kind: Abstract},
        );
      let m =
        utyp_to_info_map(
          tbody |> TypSlice.t_of_typ_t,
          ~ctx=body_ctx,
          ~ancestors,
          ~expects=TypeExpected,
          m,
        )
        |> snd;
      let m = utpat_to_info_map(~ctx, ~ancestors, utpat, m) |> snd;
      add(m); // TODO: check with andrew
    | Forall(utpat, tbody) =>
      let m =
        utyp_to_info_map(
          tbody |> TypSlice.t_of_typ_t,
          ~ctx,
          ~ancestors,
          ~expects=TypeExpected,
          m,
        )
        |> snd;
      let m = utpat_to_info_map(~ctx, ~ancestors, utpat, m) |> snd;
      add(m); // TODO: check with andrew
    | Rec({term: Var(name), _} as utpat, tbody) =>
      let body_ctx =
        Ctx.extend_tvar(
          ctx,
          {name, id: TPat.rep_id(utpat), kind: Abstract},
        );
      let m =
        utyp_to_info_map(
          tbody |> TypSlice.t_of_typ_t,
          ~ctx=body_ctx,
          ~ancestors,
          ~expects=TypeExpected,
          m,
        )
        |> snd;
      let m = utpat_to_info_map(~ctx, ~ancestors, utpat, m) |> snd;
      add(m); // TODO: check with andrew
    | Rec(utpat, tbody) =>
      let m =
        utyp_to_info_map(
          tbody |> TypSlice.t_of_typ_t,
          ~ctx,
          ~ancestors,
          ~expects=TypeExpected,
          m,
        )
        |> snd;
      let m = utpat_to_info_map(~ctx, ~ancestors, utpat, m) |> snd;
      add(m); // TODO: check with andrew
    };
  let f_slc = (term: TypSlice.slc_typ_term) =>
    switch (term) {
    | List(t)
    | Parens(t) => add(go(t, m) |> snd)
    | Arrow(t1, t2) =>
      let m = go(t1, m) |> snd;
      let m = go(t2, m) |> snd;
      add(m);
    | Prod(ts) =>
      let m = map_m(go, ts, m) |> snd;
      add(m);
    | Ap(t1, t2) =>
      let t1_mode: Info.typ_expects =
        switch (expects) {
        | VariantExpected(m, sum_ty) =>
          ConstructorExpected(
            m,
            `SliceIncr((
              Slice(Arrow(t2, sum_ty)),
              TypSlice.empty_slice_incr,
            ))
            |> TypSlice.temp,
          )
        | _ =>
          ConstructorExpected(
            Unique,
            `SliceIncr((
              Slice(Arrow(t2, `Typ(Unknown(Internal)) |> TypSlice.temp)),
              TypSlice.empty_slice_incr,
            ))
            |> TypSlice.temp,
          )
        };
      let m = go'(~expects=t1_mode, t1, m) |> snd;
      let m = go'(~expects=TypeExpected, t2, m) |> snd;
      add(m);
    | Sum(variants) =>
      let (m, _) =
        List.fold_left(
          variant_to_info_map(~ctx, ~ancestors, ~ty_sum=utyp),
          (m, []),
          variants,
        );
      add(m);
    | Forall({term: Var(name), _} as utpat, tbody) =>
      let body_ctx =
        Ctx.extend_tvar(
          ctx,
          {name, id: TPat.rep_id(utpat), kind: Abstract},
        );
      let m =
        utyp_to_info_map(
          tbody,
          ~ctx=body_ctx,
          ~ancestors,
          ~expects=TypeExpected,
          m,
        )
        |> snd;
      let m = utpat_to_info_map(~ctx, ~ancestors, utpat, m) |> snd;
      add(m); // TODO: check with andrew
    | Forall(utpat, tbody) =>
      let m =
        utyp_to_info_map(tbody, ~ctx, ~ancestors, ~expects=TypeExpected, m)
        |> snd;
      let m = utpat_to_info_map(~ctx, ~ancestors, utpat, m) |> snd;
      add(m); // TODO: check with andrew
    | Rec({term: Var(name), _} as utpat, tbody) =>
      let body_ctx =
        Ctx.extend_tvar(
          ctx,
          {name, id: TPat.rep_id(utpat), kind: Abstract},
        );
      let m =
        utyp_to_info_map(
          tbody,
          ~ctx=body_ctx,
          ~ancestors,
          ~expects=TypeExpected,
          m,
        )
        |> snd;
      let m = utpat_to_info_map(~ctx, ~ancestors, utpat, m) |> snd;
      add(m); // TODO: check with andrew
    | Rec(utpat, tbody) =>
      let m =
        utyp_to_info_map(tbody, ~ctx, ~ancestors, ~expects=TypeExpected, m)
        |> snd;
      let m = utpat_to_info_map(~ctx, ~ancestors, utpat, m) |> snd;
      add(m); // TODO: check with andrew
    };
  TypSlice.apply(f_typ, f_slc, term);
}
and utpat_to_info_map =
    (~ctx, ~ancestors, {ids, term, _} as utpat: TPat.t, m: Map.t)
    : (Info.tpat, Map.t) => {
  let add = m => {
    let info = Info.derived_tpat(~utpat, ~ctx, ~ancestors);
    (info, add_info(ids, InfoTPat(info), m));
  };
  let ancestors = [TPat.rep_id(utpat)] @ ancestors;
  switch (term) {
  | MultiHole(tms) =>
    let (_, m) = multi(~ctx, ~ancestors, m, tms);
    add(m);
  | Invalid(_)
  | EmptyHole
  | Var(_) => add(m)
  };
}
and variant_to_info_map =
    (
      ~ctx,
      ~ancestors,
      ~ty_sum,
      (m, ctrs),
      uty: ConstructorMap.variant(TypSlice.t),
    ) => {
  let go = expects => utyp_to_info_map(~ctx, ~ancestors, ~expects);
  switch (uty) {
  | BadEntry(uty) =>
    let m = go(VariantExpected(Unique, ty_sum), uty, m) |> snd;
    (m, ctrs);
  | Variant(ctr, ids, param) =>
    let m =
      go(
        ConstructorExpected(
          List.mem(ctr, ctrs) ? Duplicate : Unique,
          ty_sum,
        ),
        {term: `Typ(Var(ctr)), ids, copied: false},
        m,
      )
      |> snd;
    let m =
      switch (param) {
      | Some(param_ty) => go(TypeExpected, param_ty, m) |> snd
      | None => m
      };
    (m, [ctr, ...ctrs]);
  };
};

let mk =
  Core.Memo.general(~cache_size_bound=1000, (ctx, e) => {
    uexp_to_info_map(~ctx, ~ancestors=[], e, Id.Map.empty) |> snd
  });

let mk = (core: CoreSettings.t, ctx, exp) =>
  core.statics ? mk(ctx, exp) : Id.Map.empty;

let get_error_at = (info_map: Map.t, id: Id.t) => {
  id
  |> Id.Map.find_opt(_, info_map)
  |> Option.bind(
       _,
       fun
       | InfoExp(e) => Some(e)
       | _ => None,
     )
  |> Option.bind(_, e =>
       switch (e.status) {
       | InHole(err_info) => Some(err_info)
       | NotInHole(_) => None
       }
     );
};

let get_pat_error_at = (info_map: Map.t, id: Id.t) => {
  id
  |> Id.Map.find_opt(_, info_map)
  |> Option.bind(
       _,
       fun
       | InfoPat(e) => Some(e)
       | _ => None,
     )
  |> Option.bind(_, e =>
       switch (e.status) {
       | InHole(err_info) => Some(err_info)
       | NotInHole(_) => None
       }
     );
};

let collect_errors = (map: Map.t): list((Id.t, Info.error)) =>
  Id.Map.fold(
    (id, info: Info.t, acc) =>
      Option.to_list(Info.error_of(info) |> Option.map(x => (id, x))) @ acc,
    map,
    [],
  );
