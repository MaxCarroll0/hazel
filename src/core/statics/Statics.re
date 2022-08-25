open Sexplib.Std;

/* STATICS

     This module determines the statics semantics of the language.
     It takes a term and returns a map which associates the unique
     ids of each term to an 'info' data structure which reflects that
     term's statics. The statics collected depend on the term's sort,
     but every term has a syntactic class (The cls types from Term),
     except Invalid terms which Term could not parse.

     The map generated by this module is intended to be generated once
     from a given term and then reused anywhere there is logic which
     depends on static information.
   */

/* Expressions are assigned a mode (reflecting the static expectations
   if any of their syntactic parent), a self (reflecting what their
   statics would be in isolation), a context (variables in scope), and
   free (variables occuring free in the expression. */
[@deriving (show({with_path: false}), sexp, yojson)]
type info_exp = {
  cls: Term.UExp.cls,
  term: Term.UExp.t,
  mode: Typ.mode,
  self: Typ.self,
  ctx: Ctx.t,
  free: Ctx.co,
  // TODO: add derived attributes like error_status and typ_after_fix?
};

/* Patterns are assigned a mode (reflecting the static expectations
   if any of their syntactic parent) and a self (reflecting what their
   statics would be in isolation), a context (variables in scope) */
[@deriving (show({with_path: false}), sexp, yojson)]
type info_pat = {
  cls: Term.UPat.cls,
  term: Term.UPat.t,
  mode: Typ.mode,
  self: Typ.self,
  ctx: Ctx.t // TODO: detect in-pattern shadowing
};

/* (Syntactic) Types are assigned their corresponding semantic type. */
[@deriving (show({with_path: false}), sexp, yojson)]
type info_typ = {
  cls: Term.UTyp.cls,
  term: Term.UTyp.t,
  ty: Typ.t,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type info_rul = {cls: Term.URul.cls};

/* The Info aka Cursorinfo assigned to each subterm. */
[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Invalid(Term.parse_flag)
  | InfoExp(info_exp)
  | InfoPat(info_pat)
  | InfoTyp(info_typ)
  | InfoRul(info_rul);

/* The InfoMap collating all info for a composite term */
type map = Id.Map.t(t);

/* Static error classes */
[@deriving (show({with_path: false}), sexp, yojson)]
type error =
  | FreeVariable
  | Multi
  | SynInconsistentBranches(list(Typ.t))
  | TypeInconsistent(Typ.t, Typ.t);

/* Statics non-error classes */
[@deriving (show({with_path: false}), sexp, yojson)]
type happy =
  | SynConsistent(Typ.t)
  | AnaConsistent(Typ.t, Typ.t, Typ.t) //ana, syn, join
  | AnaInternalInconsistent(Typ.t, list(Typ.t)) // ana, branches
  | AnaExternalInconsistent(Typ.t, Typ.t); // ana, syn

/* The error status which 'wraps' each term. */
[@deriving (show({with_path: false}), sexp, yojson)]
type error_status =
  | InHole(error)
  | NotInHole(happy);

/* Determines whether an expression or pattern is in an error hole,
   depending on the mode, which represents the expectations of the
   surrounding syntactic context, and the self which represents the
   makeup of the expression / pattern itself. */
let error_status = (mode: Typ.mode, self: Typ.self): error_status =>
  switch (mode, self) {
  | (Syn | Ana(_), Free) => InHole(FreeVariable)
  | (Syn | Ana(_), Multi) => InHole(Multi)
  | (Syn, Just(ty)) => NotInHole(SynConsistent(ty))
  | (Syn, Joined(tys_syn)) =>
    /*| (Ana(Unknown(SynSwitch)), Joined(tys_syn))*/
    // Above can be commented out if we actually switch to syn on synswitch
    let tys_syn = Typ.source_tys(tys_syn);
    switch (Typ.join_all(tys_syn)) {
    | None => InHole(SynInconsistentBranches(tys_syn))
    | Some(ty_joined) => NotInHole(SynConsistent(ty_joined))
    };

  | (Ana(ty_ana), Just(ty_syn)) =>
    switch (Typ.join(ty_ana, ty_syn)) {
    | None => InHole(TypeInconsistent(ty_syn, ty_ana))
    | Some(ty_join) => NotInHole(AnaConsistent(ty_ana, ty_syn, ty_join))
    }
  | (Ana(ty_ana), Joined(tys_syn)) =>
    // TODO: review logic of these cases
    switch (Typ.join_all(Typ.source_tys(tys_syn))) {
    | Some(ty_syn) =>
      switch (Typ.join(ty_syn, ty_ana)) {
      | None => NotInHole(AnaExternalInconsistent(ty_ana, ty_syn))
      | Some(ty_join) => NotInHole(AnaConsistent(ty_syn, ty_ana, ty_join))
      }
    | None =>
      NotInHole(AnaInternalInconsistent(ty_ana, Typ.source_tys(tys_syn)))
    }
  };

/* Determines whether any term is in an error hole. Currently types cannot
   be in error, and Invalids (things to which Term was unable to assign a
   parse) are always in error. The error status of expressions and patterns
   are determined by error_status above. */
let is_error = (ci: t): bool => {
  switch (ci) {
  | Invalid(Whitespace) => false
  | Invalid(_) => true
  | InfoExp({mode, self, _})
  | InfoPat({mode, self, _}) =>
    switch (error_status(mode, self)) {
    | InHole(_) => true
    | NotInHole(_) => false
    }
  | InfoTyp(_) => false
  | InfoRul(_) => false //TODO
  };
};

/* Determined the type of an expression or pattern 'after hole wrapping';
   that is, all ill-typed terms are considered to be 'wrapped in
   non-empty holes', i.e. assigned Unknown type. */
let typ_after_fix = (mode: Typ.mode, self: Typ.self): Typ.t =>
  switch (error_status(mode, self)) {
  | InHole(_) => Unknown(Internal)
  | NotInHole(SynConsistent(t)) => t
  | NotInHole(AnaConsistent(_, _, ty_join)) => ty_join
  | NotInHole(AnaExternalInconsistent(ty_ana, _)) => ty_ana
  | NotInHole(AnaInternalInconsistent(ty_ana, _)) => ty_ana
  };

/* The type of an expression after hole wrapping */
let exp_typ = (m: map, e: Term.UExp.t): Typ.t =>
  switch (Id.Map.find_opt(e.id, m)) {
  | Some(InfoExp({mode, self, _})) => typ_after_fix(mode, self)
  | Some(InfoPat(_) | InfoTyp(_) | InfoRul(_) | Invalid(_))
  | None => failwith(__LOC__ ++ ": XXX")
  };

/* The type of a pattern after hole wrapping */
let pat_typ = (m: map, p: Term.UPat.t): Typ.t =>
  switch (Id.Map.find_opt(p.id, m)) {
  | Some(InfoPat({mode, self, _})) => typ_after_fix(mode, self)
  | Some(InfoExp(_) | InfoTyp(_) | InfoRul(_) | Invalid(_))
  | None => failwith(__LOC__ ++ ": XXX")
  };

let union_m =
  List.fold_left(
    (m1, m2) => Id.Map.union((_, _, b) => Some(b), m1, m2),
    Id.Map.empty,
  );

let extend_let_def_ctx =
    (ctx: Ctx.t, pat: Term.UPat.t, def: Term.UExp.t, ty_ann: Typ.t) =>
  switch (ty_ann, pat.term, def.term) {
  | (Arrow(_), Var(x) | TypeAnn({term: Var(x), _}, _), Fun(_)) =>
    VarMap.extend(ctx, (x, {id: pat.id, typ: ty_ann}))
  | _ => ctx
  };

let typ_exp_binop_bin_int: Term.UExp.op_bin_int => Typ.t =
  fun
  | (Plus | Minus | Times | Divide) as _op => Int
  | (LessThan | GreaterThan | Equals) as _op => Bool;

let typ_exp_binop_bin_float: Term.UExp.op_bin_float => Typ.t =
  fun
  | (Plus | Minus | Times | Divide) as _op => Float
  | (LessThan | GreaterThan | Equals) as _op => Bool;

let typ_exp_binop: Term.UExp.op_bin => (Typ.t, Typ.t, Typ.t) =
  fun
  | Bool(And | Or) => (Bool, Bool, Bool)
  | Int(op) => (Int, Int, typ_exp_binop_bin_int(op))
  | Float(op) => (Float, Float, typ_exp_binop_bin_float(op));

let typ_exp_unop: Term.UExp.op_un => (Typ.t, Typ.t) =
  fun
  | Int(Minus) => (Int, Int);

let rec uexp_to_info_map =
        (~ctx: Ctx.t, ~mode=Typ.Syn, {id, term} as uexp: Term.UExp.t)
        : (Typ.t, Ctx.co, map) => {
  /* Maybe switch mode to syn */
  let mode =
    switch (mode) {
    | Ana(Unknown(SynSwitch)) => Typ.Syn
    | _ => mode
    };
  let cls = Term.UExp.cls_of_term(term);
  let go = uexp_to_info_map(~ctx);
  let add = (~self, ~free, m) => (
    typ_after_fix(mode, self),
    free,
    Id.Map.add(id, InfoExp({cls, self, mode, ctx, free, term: uexp}), m),
  );
  let atomic = self => add(~self, ~free=[], Id.Map.empty);
  switch (term) {
  | Invalid(msg, _p) => (
      Unknown(Internal),
      [],
      Id.Map.singleton(id, Invalid(msg)),
    )
  | MultiHole(ids, es) =>
    let es = List.map(go(~mode=Syn), es);
    let self = Typ.Multi;
    let free = Ctx.union(List.map(((_, f, _)) => f, es));
    let info = InfoExp({cls, self, mode, ctx, free, term: uexp});
    let m = union_m(List.map(((_, _, m)) => m, es));
    let m = List.fold_left((m, id) => Id.Map.add(id, info, m), m, ids);
    (typ_after_fix(mode, self), free, m);
  | EmptyHole => atomic(Just(Unknown(Internal)))
  | Triv => atomic(Just(Prod([])))
  | Bool(_) => atomic(Just(Bool))
  | Int(_) => atomic(Just(Int))
  | Float(_) => atomic(Just(Float))
  | Var(name) =>
    switch (VarMap.lookup(ctx, name)) {
    | None => atomic(Free)
    | Some(ce) =>
      add(~self=Just(ce.typ), ~free=[(name, [{id, mode}])], Id.Map.empty)
    }
  | Parens(e) =>
    let (ty, free, m) = go(~mode, e);
    add(~self=Just(ty), ~free, m);
  | UnOp(op, e) =>
    let (ty_in, ty_out) = typ_exp_unop(op);
    let (_, free, m) = go(~mode=Ana(ty_in), e);
    add(~self=Just(ty_out), ~free, m);
  | BinOp(op, e1, e2) =>
    let (ty1, ty2, ty_out) = typ_exp_binop(op);
    let (_, free1, m1) = go(~mode=Ana(ty1), e1);
    let (_, free2, m2) = go(~mode=Ana(ty2), e2);
    add(
      ~self=Just(ty_out),
      ~free=Ctx.union([free1, free2]),
      union_m([m1, m2]),
    );
  | Tuple(ids, es) =>
    let modes = Typ.matched_prod_mode(mode, List.length(es));
    let infos = List.map2((e, mode) => go(~mode, e), es, modes);
    let free = Ctx.union(List.map(((_, f, _)) => f, infos));
    let self = Typ.Just(Prod(List.map(((ty, _, _)) => ty, infos)));
    let info = InfoExp({cls, self, mode, ctx, free, term: uexp});
    let m = union_m(List.map(((_, _, m)) => m, infos));
    /* Add an entry for the id of each comma tile */
    let m = List.fold_left((m, id) => Id.Map.add(id, info, m), m, ids);
    (typ_after_fix(mode, self), free, m);
  | Cons(e1, e2) =>
    let mode_ele = Typ.matched_list_mode(mode);
    let (ty1, free1, m1) = go(~mode=mode_ele, e1);
    let (_, free2, m2) = go(~mode=Ana(List(ty1)), e2);
    add(
      ~self=Just(List(ty1)),
      ~free=Ctx.union([free1, free2]),
      union_m([m1, m2]),
    );
  | ListLit([], []) => atomic(Just(List(Unknown(Internal))))
  | ListLit(ids, es) =>
    //TODO(andrew) LISTLITS: below is placeholder logic, might be messy/wrong/incomplete
    let modes = Typ.matched_list_lit_mode(mode, List.length(es));
    let e_ids = List.map((e: Term.UExp.t) => e.id, es);
    let infos = List.map2((e, mode) => go(~mode, e), es, modes);
    let tys = List.map(((ty, _, _)) => ty, infos);
    let self: Typ.self =
      switch (Typ.join_all(tys)) {
      | None => Joined(List.map2((id, ty) => Typ.{id, ty}, e_ids, tys))
      | Some(ty) => Just(List(ty))
      };
    let free = Ctx.union(List.map(((_, f, _)) => f, infos));
    let info = InfoExp({cls, self, mode, ctx, free, term: uexp});
    let m = union_m(List.map(((_, _, m)) => m, infos));
    /* Add an entry for the id of each comma tile */
    let m = List.fold_left((m, id) => Id.Map.add(id, info, m), m, ids);
    (typ_after_fix(mode, self), free, m);
  | Test(test) =>
    let (_, free_test, m1) = go(~mode=Ana(Bool), test);
    add(~self=Just(Prod([])), ~free=free_test, m1);
  | If(cond, e1, e2) =>
    let (_, free_e0, m1) = go(~mode=Ana(Bool), cond);
    let (ty_e1, free_e1, m2) = go(~mode, e1);
    let (ty_e2, free_e2, m3) = go(~mode, e2);
    add(
      ~self=Joined([{id: e1.id, ty: ty_e1}, {id: e2.id, ty: ty_e2}]),
      ~free=Ctx.union([free_e0, free_e1, free_e2]),
      union_m([m1, m2, m3]),
    );
  | Seq(e1, e2) =>
    let (_, free1, m1) = go(~mode=Syn, e1);
    let (ty2, free2, m2) = go(~mode, e2);
    add(
      ~self=Just(ty2),
      ~free=Ctx.union([free1, free2]),
      union_m([m1, m2]),
    );
  | Ap(fn, arg) =>
    /* Function position mode Ana(Hole->Hole) instead of Syn */
    let (ty_fn, free_fn, m_fn) =
      uexp_to_info_map(~ctx, ~mode=Typ.ap_mode, fn);
    let (ty_in, ty_out) = Typ.matched_arrow(ty_fn);
    let (_, free_arg, m_arg) =
      uexp_to_info_map(~ctx, ~mode=Ana(ty_in), arg);
    add(
      ~self=Just(ty_out),
      ~free=Ctx.union([free_fn, free_arg]),
      union_m([m_fn, m_arg]),
    );
  | Fun(pat, body) =>
    let (mode_pat, mode_body) = Typ.matched_arrow_mode(mode);
    let (ty_pat, ctx_pat, m_pat) = upat_to_info_map(~mode=mode_pat, pat);
    let ctx_body = VarMap.union(ctx_pat, ctx);
    let (ty_body, free_body, m_body) =
      uexp_to_info_map(~ctx=ctx_body, ~mode=mode_body, body);
    add(
      ~self=Just(Arrow(ty_pat, ty_body)),
      ~free=Ctx.subtract(ctx_pat, free_body),
      union_m([m_pat, m_body]),
    );
  | Let(pat, def, body) =>
    let (ty_pat, _ctx_pat, _m_pat) = upat_to_info_map(~mode=Syn, pat);
    let def_ctx = extend_let_def_ctx(ctx, pat, def, ty_pat);
    let (ty_def, free_def, m_def) =
      uexp_to_info_map(~ctx=def_ctx, ~mode=Ana(ty_pat), def);
    /* Analyze pattern to incorporate def type into ctx */
    let (_, ctx_pat_ana, m_pat) = upat_to_info_map(~mode=Ana(ty_def), pat);
    let ctx_body = VarMap.union(ctx_pat_ana, def_ctx);
    let (ty_body, free_body, m_body) =
      uexp_to_info_map(~ctx=ctx_body, ~mode, body);
    add(
      ~self=Just(ty_body),
      ~free=Ctx.union([free_def, Ctx.subtract(ctx_pat_ana, free_body)]),
      union_m([m_pat, m_def, m_body]),
    );
  | Match(ids, scrut, rules) =>
    let (ty_scrut, free_scrut, m_scrut) = go(~mode=Syn, scrut);
    let (pats, branches) = List.split(rules);
    let pat_infos =
      List.map(upat_to_info_map(~mode=Typ.Ana(ty_scrut)), pats);
    let branch_infos =
      List.map2(
        (branch, (_, ctx_pat, _)) =>
          uexp_to_info_map(~ctx=VarMap.union(ctx_pat, ctx), ~mode, branch),
        branches,
        pat_infos,
      );
    let branch_sources =
      List.map2(
        ({id, _}: Term.UExp.t, (ty, _, _)) => Typ.{id, ty},
        branches,
        branch_infos,
      );
    let rule_ms =
      List.fold_left(
        (m, id) => Id.Map.add(id, InfoRul({cls: Rule}), m),
        Id.Map.empty,
        ids,
      );
    let pat_ms = List.map(((_, _, m)) => m, pat_infos);
    let branch_ms = List.map(((_, _, m)) => m, branch_infos);
    let branch_frees = List.map(((_, free, _)) => free, branch_infos);
    add(
      ~self=Joined(branch_sources),
      ~free=Ctx.union([free_scrut] @ branch_frees),
      union_m([rule_ms, m_scrut] @ pat_ms @ branch_ms),
    );
  };
}
and upat_to_info_map =
    (
      ~ctx=Ctx.empty,
      ~mode: Typ.mode=Typ.Syn,
      {id, term} as upat: Term.UPat.t,
    )
    : (Typ.t, Ctx.t, map) => {
  let cls = Term.UPat.cls_of_term(term);
  let add = (~self, ~ctx, m) => (
    typ_after_fix(mode, self),
    ctx,
    Id.Map.add(id, InfoPat({cls, self, mode, ctx, term: upat}), m),
  );
  let atomic = self => add(~self, ~ctx, Id.Map.empty);
  let unknown = Typ.Just(Unknown(SynSwitch));
  switch (term) {
  | Invalid(msg, _) => (
      Unknown(Internal),
      ctx,
      Id.Map.singleton(id, Invalid(msg)),
    )
  | MultiHole(ids, ps) =>
    let ps = List.map(upat_to_info_map(~ctx, ~mode=Syn), ps);
    let self = Typ.Multi;
    let info: t = InfoPat({cls, self, mode, ctx, term: upat});
    let m = union_m(List.map(((_, _, m)) => m, ps));
    let m = List.fold_left((m, id) => Id.Map.add(id, info, m), m, ids);
    (typ_after_fix(mode, self), ctx, m);
  | EmptyHole
  | Wild => atomic(unknown)
  | Int(_) => atomic(Just(Int))
  | Float(_) => atomic(Just(Float))
  | Triv => atomic(Just(Prod([])))
  | Bool(_) => atomic(Just(Bool))
  | ListLit(ids, ps) =>
    let modes = Typ.matched_prod_mode(mode, List.length(ps));
    let (ctx, infos) =
      List.fold_left2(
        ((ctx, infos), e, mode) => {
          let (_, ctx, _) as info = upat_to_info_map(~mode, ~ctx, e);
          (ctx, infos @ [info]);
        },
        (ctx, []),
        ps,
        modes,
      );
    let self = Typ.Just(List(Unknown(Internal)));
    let info: t = InfoPat({cls, self, mode, ctx, term: upat});
    let m = union_m(List.map(((_, _, m)) => m, infos));
    /* Add an entry for the id of each comma tile */
    let m = List.fold_left((m, id) => Id.Map.add(id, info, m), m, ids);
    (typ_after_fix(mode, self), ctx, m);
  | Var(name) =>
    let self = unknown;
    let typ = typ_after_fix(mode, self);
    add(~self, ~ctx=VarMap.extend(ctx, (name, {id, typ})), Id.Map.empty);
  | Tuple(ids, ps) =>
    let modes = Typ.matched_prod_mode(mode, List.length(ps));
    let (ctx, infos) =
      List.fold_left2(
        ((ctx, infos), e, mode) => {
          let (_, ctx, _) as info = upat_to_info_map(~mode, ~ctx, e);
          (ctx, infos @ [info]);
        },
        (ctx, []),
        ps,
        modes,
      );
    let self = Typ.Just(Prod(List.map(((ty, _, _)) => ty, infos)));
    let info: t = InfoPat({cls, self, mode, ctx, term: upat});
    let m = union_m(List.map(((_, _, m)) => m, infos));
    /* Add an entry for the id of each comma tile */
    let m = List.fold_left((m, id) => Id.Map.add(id, info, m), m, ids);
    (typ_after_fix(mode, self), ctx, m);
  | Parens(p) =>
    let (ty, ctx, m) = upat_to_info_map(~ctx, ~mode, p);
    add(~self=Just(ty), ~ctx, m);
  | TypeAnn(p, ty) =>
    let (ty_ann, m_typ) = utyp_to_info_map(ty);
    let (_ty, ctx, m) = upat_to_info_map(~ctx, ~mode=Ana(ty_ann), p);
    add(~self=Just(ty_ann), ~ctx, union_m([m, m_typ]));
  };
}
and utyp_to_info_map = ({id, term} as utyp: Term.UTyp.t): (Typ.t, map) => {
  let cls = Term.UTyp.cls_of_term(term);
  let ty = Term.utyp_to_ty(utyp);
  let add = (m, id) => Id.Map.add(id, InfoTyp({cls, ty, term: utyp}), m);
  let return = m => (ty, add(m, id));
  switch (term) {
  | Invalid(msg, _) => (
      Unknown(Internal),
      Id.Map.singleton(id, Invalid(msg)),
    )
  | EmptyHole
  | Int
  | Float
  | Bool => return(Id.Map.empty)
  | List(t)
  | Parens(t) =>
    let (_, m) = utyp_to_info_map(t);
    return(m);
  | Arrow(t1, t2) =>
    let (_, m_t1) = utyp_to_info_map(t1);
    let (_, m_t2) = utyp_to_info_map(t2);
    return(union_m([m_t1, m_t2]));
  | MultiHole(ids, ts)
  | Tuple(ids, ts) =>
    let m = ts |> List.map(utyp_to_info_map) |> List.map(snd) |> union_m;
    let m = List.fold_left(add, m, ids);
    (ty, m);
  };
};

let mk_map =
  Core_kernel.Memo.general(
    ~cache_size_bound=1000,
    uexp_to_info_map(~ctx=Ctx.empty),
  );
