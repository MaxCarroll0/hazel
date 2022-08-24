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
  mode: Typ.mode,
  self: Typ.self,
  ctx: Typ.Ctx.t,
  free: Typ.Ctx.co,
  // TODO: add derived attributes like error_status and typ_after_fix?
};

/* Patterns are assigned a mode (reflecting the static expectations
   if any of their syntactic parent) and a self (reflecting what their
   statics would be in isolation), a context (variables in scope) */
[@deriving (show({with_path: false}), sexp, yojson)]
type info_pat = {
  cls: Term.UPat.cls,
  mode: Typ.mode,
  self: Typ.self,
  ctx: Typ.Ctx.t // TODO: detect in-pattern shadowing
};

/* (Syntactic) Types are assigned their corresponding semantic type. */
[@deriving (show({with_path: false}), sexp, yojson)]
type info_typ = {
  cls: Term.UTyp.cls,
  mode: Typ.mode,
  self: Typ.self,
  ctx: Typ.Ctx.t,
  ty: Typ.t,
  free: Typ.Ctx.co,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type info_tpat = {
  cls: Term.UTPat.cls,
  mode: Typ.mode,
  self: Typ.self,
  ctx: Typ.Ctx.t,
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
  | InfoTPat(info_tpat)
  | InfoRul(info_rul);

/* The InfoMap collating all info for a composite term */
type map = Id.Map.t(t);

let sexp_of_map = (map: map): Sexplib.Sexp.t =>
  Id.Map.bindings(map)
  |> Sexplib.Std.sexp_of_list(((id, info)) =>
       List([Sexplib.Std.sexp_of_int(id), sexp_of_t(info)])
     );

let map_of_sexp = (sexp: Sexplib.Sexp.t): map =>
  Sexplib.Std.list_of_sexp(
    s =>
      switch (s) {
      | List([id_sexp, info_sexp]) =>
        let id = Sexplib.Std.int_of_sexp(id_sexp);
        let info = t_of_sexp(info_sexp);
        (id, info);
      | _ => Sexplib.Conv_error.no_variant_match()
      },
    sexp,
  )
  |> List.to_seq
  |> Id.Map.of_seq;

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
let error_status =
    (ctx: Typ.Ctx.t, mode: Typ.mode, self: Typ.self): error_status =>
  switch (mode, self) {
  | (Syn | Ana(_), Free) => InHole(FreeVariable)
  | (Syn | Ana(_), Multi) => InHole(Multi)
  | (Syn, Just(ty)) => NotInHole(SynConsistent(ty))
  | (Syn, Joined(tys_syn)) =>
    /*| (Ana(Unknown(SynSwitch)), Joined(tys_syn))*/
    // Above can be commented out if we actually switch to syn on synswitch
    let tys_syn = Typ.source_tys(tys_syn);
    switch (Typ.join_all(ctx, tys_syn)) {
    | None => InHole(SynInconsistentBranches(tys_syn))
    | Some(ty_joined) => NotInHole(SynConsistent(ty_joined))
    };

  | (Ana(ty_ana), Just(ty_syn)) =>
    switch (Typ.join(ctx, ty_ana, ty_syn)) {
    | None => InHole(TypeInconsistent(ty_syn, ty_ana))
    | Some(ty_join) => NotInHole(AnaConsistent(ty_ana, ty_syn, ty_join))
    }
  | (Ana(ty_ana), Joined(tys_syn)) =>
    // TODO: review logic of these cases
    switch (Typ.join_all(ctx, Typ.source_tys(tys_syn))) {
    | Some(ty_syn) =>
      switch (Typ.join(ctx, ty_syn, ty_ana)) {
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
  | InfoExp({mode, self, ctx, _})
  | InfoPat({mode, self, ctx, _}) =>
    switch (error_status(ctx, mode, self)) {
    | InHole(_) => true
    | NotInHole(_) => false
    }
  | InfoTyp(_) => false
  | InfoTPat({mode, self, ctx, _}) =>
    switch (error_status(ctx, mode, self)) {
    | InHole(_) => true
    | NotInHole(_) => false
    }
  | InfoRul(_) => false //TODO
  };
};

/* Converts a syntactic type into a semantic type */
let rec utyp_to_ty = (ctx: Typ.Ctx.t, utyp: Term.UTyp.t): Typ.t =>
  switch (utyp.term) {
  | Invalid(_)
  | MultiHole(_) => Typ.unknown(Internal)
  | EmptyHole => Typ.unknown(TypeHole)
  | Bool => Typ.bool()
  | Int => Typ.int()
  | Float => Typ.float()
  | Var(t) =>
    switch (Typ.Ctx.tyvar_named(ctx, t)) {
    | Some((cref, _)) => Typ.tyvar(ctx, Some(cref.index), t)
    | None => Typ.tyvar(ctx, None, t)
    }
  | Arrow(u1, u2) => Typ.arrow(utyp_to_ty(ctx, u1), utyp_to_ty(ctx, u2))
  | Tuple(_, us) => Typ.product(List.map(utyp_to_ty(ctx), us))
  | List(u) => Typ.list(utyp_to_ty(ctx, u))
  | Parens(u) => utyp_to_ty(ctx, u)
  };

/* Determined the type of an expression or pattern 'after hole wrapping';
   that is, all ill-typed terms are considered to be 'wrapped in
   non-empty holes', i.e. assigned Unknown type. */
let typ_after_fix = (ctx: Typ.Ctx.t, mode: Typ.mode, self: Typ.self): Typ.t =>
  switch (error_status(ctx, mode, self)) {
  | InHole(_) => Typ.unknown(Internal)
  | NotInHole(SynConsistent(t)) => t
  | NotInHole(AnaConsistent(_, _, ty_join)) => ty_join
  | NotInHole(AnaExternalInconsistent(ty_ana, _)) => ty_ana
  | NotInHole(AnaInternalInconsistent(ty_ana, _)) => ty_ana
  };

/* The type of an expression after hole wrapping */
let exp_typ = (m: map, e: Term.UExp.t): Typ.t =>
  switch (Id.Map.find_opt(e.id, m)) {
  | Some(InfoExp({mode, self, ctx, _})) => typ_after_fix(ctx, mode, self)
  | Some(InfoPat(_) | InfoTyp(_) | InfoTPat(_) | InfoRul(_) | Invalid(_))
  | None => failwith(__LOC__ ++ ": XXX")
  };

/* The type of a pattern after hole wrapping */
let pat_typ = (m: map, p: Term.UPat.t): Typ.t =>
  switch (Id.Map.find_opt(p.id, m)) {
  | Some(InfoPat({mode, self, ctx, _})) => typ_after_fix(ctx, mode, self)
  | Some(InfoExp(_) | InfoTyp(_) | InfoTPat(_) | InfoRul(_) | Invalid(_))
  | None => failwith(__LOC__ ++ ": XXX")
  };

let union_m =
  List.fold_left(
    (m1, m2) => Id.Map.union((_, _, b) => Some(b), m1, m2),
    Id.Map.empty,
  );

let extend_let_def_ctx =
    (ctx: Typ.Ctx.t, pat: Term.UPat.t, def: Term.UExp.t, ty_ann: Typ.t) =>
  switch (Typ.to_syntax(ty_ann), pat.term, def.term) {
  | (Arrow(_), Var(x) | TypeAnn({term: Var(x), _}, _), Fun(_)) =>
    Typ.Ctx.add_var(ctx, {id: pat.id, name: x, typ: ty_ann})
  | _ => ctx
  };

let typ_exp_binop_bin_int: Term.UExp.op_bin_int => Typ.t =
  fun
  | (Plus | Minus | Times | Divide) as _op => Typ.int()
  | (LessThan | GreaterThan | Equals) as _op => Typ.bool();

let typ_exp_binop_bin_float: Term.UExp.op_bin_float => Typ.t =
  fun
  | (Plus | Minus | Times | Divide) as _op => Typ.int()
  | (LessThan | GreaterThan | Equals) as _op => Typ.bool();

let typ_exp_binop: Term.UExp.op_bin => (Typ.t, Typ.t, Typ.t) =
  fun
  | Bool(And | Or) => (Typ.bool(), Typ.bool(), Typ.bool())
  | Int(op) => (Typ.int(), Typ.int(), typ_exp_binop_bin_int(op))
  | Float(op) => (Typ.float(), Typ.float(), typ_exp_binop_bin_float(op));

let typ_exp_unop: Term.UExp.op_un => (Typ.t, Typ.t) =
  fun
  | Int(Minus) => (Typ.int(), Typ.int());

let rec uexp_to_info_map =
        (~ctx: Typ.Ctx.t, ~mode=Typ.Syn, {id, term}: Term.UExp.t)
        : (Typ.t, Typ.Ctx.co, map) => {
  /* Maybe switch mode to syn */
  let mode =
    switch (mode) {
    | Ana(ty) when Typ.to_syntax(ty) == Unknown(SynSwitch) => Typ.Syn
    | _ => mode
    };
  let cls = Term.UExp.cls_of_term(term);
  let go = uexp_to_info_map(~ctx);
  let add = (~ctx, ~self, ~free, m) => (
    typ_after_fix(ctx, mode, self),
    free,
    Id.Map.add(id, InfoExp({cls, self, mode, ctx, free}), m),
  );
  let atomic = (ctx, self) => add(~ctx, ~self, ~free=[], Id.Map.empty);
  switch (term) {
  | Invalid(msg, _p) => (
      Typ.unknown(Internal),
      [],
      Id.Map.singleton(id, Invalid(msg)),
    )
  | MultiHole(ids, es) =>
    let es = List.map(go(~mode=Syn), es);
    let self = Typ.Multi;
    let free = Typ.Ctx.union(List.map(((_, f, _)) => f, es));
    let info = InfoExp({cls, self, mode, ctx, free});
    let m = union_m(List.map(((_, _, m)) => m, es));
    let m = List.fold_left((m, id) => Id.Map.add(id, info, m), m, ids);
    (typ_after_fix(ctx, mode, self), free, m);
  | EmptyHole => atomic(ctx, Just(Typ.unknown(Internal)))
  | Triv => atomic(ctx, Just(Typ.product([])))
  | Bool(_) => atomic(ctx, Just(Typ.bool()))
  | Int(_) => atomic(ctx, Just(Typ.int()))
  | Float(_) => atomic(ctx, Just(Typ.float()))
  | Var(name) =>
    switch (Typ.Ctx.var_named(ctx, name)) {
    | None => atomic(ctx, Free)
    | Some((_, entry)) =>
      add(
        ~ctx,
        ~self=Just(entry.typ),
        ~free=[(name, [{id, mode}])],
        Id.Map.empty,
      )
    }
  | Parens(e) =>
    let (ty, free, m) = go(~mode, e);
    add(~ctx, ~self=Just(ty), ~free, m);
  | UnOp(op, e) =>
    let (ty_in, ty_out) = typ_exp_unop(op);
    let (_, free, m) = go(~mode=Ana(ty_in), e);
    add(~ctx, ~self=Just(ty_out), ~free, m);
  | BinOp(op, e1, e2) =>
    let (ty1, ty2, ty_out) = typ_exp_binop(op);
    let (_, free1, m1) = go(~mode=Ana(ty1), e1);
    let (_, free2, m2) = go(~mode=Ana(ty2), e2);
    add(
      ~ctx,
      ~self=Just(ty_out),
      ~free=Typ.Ctx.union([free1, free2]),
      union_m([m1, m2]),
    );
  | Tuple(ids, es) =>
    let modes = Typ.matched_prod_mode(mode, List.length(es));
    let infos = List.map2((e, mode) => go(~mode, e), es, modes);
    let free = Typ.Ctx.union(List.map(((_, f, _)) => f, infos));
    let self = Typ.Just(Typ.product(List.map(((ty, _, _)) => ty, infos)));
    let info = InfoExp({cls, self, mode, ctx, free});
    let m = union_m(List.map(((_, _, m)) => m, infos));
    /* Add an entry for the id of each comma tile */
    let m = List.fold_left((m, id) => Id.Map.add(id, info, m), m, ids);
    (typ_after_fix(ctx, mode, self), free, m);
  | Cons(e1, e2) =>
    let mode_ele = Typ.matched_list_mode(mode);
    let (ty1, free1, m1) = go(~mode=mode_ele, e1);
    let (_, free2, m2) = go(~mode=Ana(Typ.list(ty1)), e2);
    add(
      ~ctx,
      ~self=Just(Typ.list(ty1)),
      ~free=Typ.Ctx.union([free1, free2]),
      union_m([m1, m2]),
    );
  | ListLit([], []) => atomic(ctx, Just(Typ.list(Typ.unknown(Internal))))
  | ListLit(ids, es) =>
    //TODO(andrew) LISTLITS: below is placeholder logic, might be messy/wrong/incomplete
    let modes = Typ.matched_list_lit_mode(mode, List.length(es));
    let e_ids = List.map((e: Term.UExp.t) => e.id, es);
    let infos = List.map2((e, mode) => go(~mode, e), es, modes);
    let tys = List.map(((ty, _, _)) => ty, infos);
    let self: Typ.self =
      switch (Typ.join_all(ctx, tys)) {
      | None => Joined(List.map2((id, ty) => Typ.{id, ty}, e_ids, tys))
      | Some(ty) => Just(Typ.list(ty))
      };
    let free = Typ.Ctx.union(List.map(((_, f, _)) => f, infos));
    let info = InfoExp({cls, self, mode, ctx, free});
    let m = union_m(List.map(((_, _, m)) => m, infos));
    /* Add an entry for the id of each comma tile */
    let m = List.fold_left((m, id) => Id.Map.add(id, info, m), m, ids);
    (typ_after_fix(ctx, mode, self), free, m);
  | Test(test) =>
    let (_, free_test, m1) = go(~mode=Ana(Typ.bool()), test);
    add(~ctx, ~self=Just(Typ.product([])), ~free=free_test, m1);
  | If(cond, e1, e2) =>
    let (_, free_e0, m1) = go(~mode=Ana(Typ.bool()), cond);
    let (ty_e1, free_e1, m2) = go(~mode, e1);
    let (ty_e2, free_e2, m3) = go(~mode, e2);
    add(
      ~ctx,
      ~self=Joined([{id: e1.id, ty: ty_e1}, {id: e2.id, ty: ty_e2}]),
      ~free=Typ.Ctx.union([free_e0, free_e1, free_e2]),
      union_m([m1, m2, m3]),
    );
  | Seq(e1, e2) =>
    let (_, free1, m1) = go(~mode=Syn, e1);
    let (ty2, free2, m2) = go(~mode, e2);
    add(
      ~ctx,
      ~self=Just(ty2),
      ~free=Typ.Ctx.union([free1, free2]),
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
      ~ctx,
      ~self=Just(ty_out),
      ~free=Typ.Ctx.union([free_fn, free_arg]),
      union_m([m_fn, m_arg]),
    );
  | Fun(pat, body) =>
    let (mode_pat, mode_body) = Typ.matched_arrow_mode(mode);
    let (ty_pat, ctx_pat, m_pat) = upat_to_info_map(~mode=mode_pat, pat);
    let ctx_body = ctx_pat @ ctx;
    let (ty_body, free_body, m_body) =
      uexp_to_info_map(~ctx=ctx_body, ~mode=mode_body, body);
    add(
      ~ctx,
      ~self=Just(Typ.arrow(ty_pat, ty_body)),
      ~free=Typ.Ctx.subtract(ctx_pat, free_body),
      union_m([m_pat, m_body]),
    );
  | TyAlias(tpat, def, body) =>
    print_endline("XXX");
    print_endline(Sexplib.Sexp.to_string_hum(Term.UTPat.sexp_of_t(tpat)));
    print_endline(Sexplib.Sexp.to_string_hum(Term.UTyp.sexp_of_t(def)));
    print_endline(Sexplib.Sexp.to_string_hum(Term.UExp.sexp_of_t(body)));
    /* synthesize the definition's type */
    let (ty_def, m_def) = utyp_to_info_map(~ctx, def);
    /* Analyze type pattern to incorporate def type into ctx */
    let (_, ctx_tpat_ana, m_tpat) =
      utpat_to_info_map(~ctx, ~mode=Typ.Ana(ty_def), tpat);
    print_endline("YYY");
    print_endline(Sexplib.Sexp.to_string_hum(sexp_of_map(m_tpat)));
    /* recur into the body */
    let ctx_body = ctx_tpat_ana @ ctx;
    let (ty_body, free_body, m_body) =
      uexp_to_info_map(~ctx=ctx_body, ~mode, body);
    let (typ, co, m) =
      add(
        ~ctx,
        ~self=Just(ty_body),
        ~free=free_body,
        union_m([m_tpat, m_def, m_body]),
      );
    print_endline("ZZZ");
    print_endline(Sexplib.Sexp.to_string_hum(Typ.sexp_of_t(typ)));
    print_endline(Sexplib.Sexp.to_string_hum(Typ.Ctx.sexp_of_co(co)));
    print_endline(Sexplib.Sexp.to_string_hum(sexp_of_map(m)));
    (typ, co, m);
  | Let(pat, def, body) =>
    let (ty_pat, _ctx_pat, _m_pat) = upat_to_info_map(~mode=Syn, pat);
    let def_ctx = extend_let_def_ctx(ctx, pat, def, ty_pat);
    let (ty_def, free_def, m_def) =
      uexp_to_info_map(~ctx=def_ctx, ~mode=Ana(ty_pat), def);
    /* Analyze pattern to incorporate def type into ctx */
    let (_, ctx_pat_ana, m_pat) = upat_to_info_map(~mode=Ana(ty_def), pat);
    let ctx_body = ctx_pat_ana @ def_ctx;
    let (ty_body, free_body, m_body) =
      uexp_to_info_map(~ctx=ctx_body, ~mode, body);
    add(
      ~ctx,
      ~self=Just(ty_body),
      ~free=free_def @ Typ.Ctx.subtract(ctx_pat_ana, free_body),
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
          uexp_to_info_map(~ctx=ctx_pat @ ctx, ~mode, branch),
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
      ~ctx,
      ~self=Joined(branch_sources),
      ~free=List.concat([free_scrut, ...branch_frees]),
      union_m([rule_ms, m_scrut] @ pat_ms @ branch_ms),
    );
  };
}
and upat_to_info_map =
    (~ctx=Typ.Ctx.empty(), ~mode: Typ.mode=Typ.Syn, {id, term}: Term.UPat.t)
    : (Typ.t, Typ.Ctx.t, map) => {
  let cls = Term.UPat.cls_of_term(term);
  let add = (~self, ~ctx, m) => (
    typ_after_fix(ctx, mode, self),
    ctx,
    Id.Map.add(id, InfoPat({cls, self, mode, ctx}), m),
  );
  let atomic = (ctx, self) => add(~self, ~ctx, Id.Map.empty);
  let unknown = Typ.Just(Typ.unknown(SynSwitch));
  switch (term) {
  | Invalid(msg, _) => (
      Typ.unknown(Internal),
      ctx,
      Id.Map.singleton(id, Invalid(msg)),
    )
  | MultiHole(ids, ps) =>
    let ps = List.map(upat_to_info_map(~ctx, ~mode=Syn), ps);
    let self = Typ.Multi;
    let info: t = InfoPat({cls, self, mode, ctx});
    let m = union_m(List.map(((_, _, m)) => m, ps));
    let m = List.fold_left((m, id) => Id.Map.add(id, info, m), m, ids);
    (typ_after_fix(ctx, mode, self), ctx, m);
  | EmptyHole
  | Wild => atomic(ctx, unknown)
  | Int(_) => atomic(ctx, Just(Typ.int()))
  | Float(_) => atomic(ctx, Just(Typ.float()))
  | Triv => atomic(ctx, Just(Typ.product([])))
  | Bool(_) => atomic(ctx, Just(Typ.bool()))
  | ListNil => atomic(ctx, Just(Typ.list(Typ.unknown(Internal))))
  | Var(name) =>
    let self = unknown;
    let typ = typ_after_fix(ctx, mode, self);
    add(~self, ~ctx=Typ.Ctx.add_var(ctx, {id, name, typ}), Id.Map.empty);
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
    let self = Typ.Just(Typ.product(List.map(((ty, _, _)) => ty, infos)));
    let info: t = InfoPat({cls, self, mode, ctx});
    let m = union_m(List.map(((_, _, m)) => m, infos));
    /* Add an entry for the id of each comma tile */
    let m = List.fold_left((m, id) => Id.Map.add(id, info, m), m, ids);
    (typ_after_fix(ctx, mode, self), ctx, m);
  | Parens(p) =>
    let (ty, ctx, m) = upat_to_info_map(~ctx, ~mode, p);
    add(~self=Just(ty), ~ctx, m);
  | TypeAnn(p, ty) =>
    let (ty_ann, m_typ) = utyp_to_info_map(~ctx, ty);
    let (_ty, ctx, m) = upat_to_info_map(~ctx, ~mode=Ana(ty_ann), p);
    add(~self=Just(ty_ann), ~ctx, union_m([m, m_typ]));
  };
}
and utyp_to_info_map =
    (
      ~ctx: Typ.Ctx.t,
      ~mode: Typ.mode=Typ.Syn,
      {id, term} as utyp: Term.UTyp.t,
    )
    : (Typ.t, Typ.Ctx.co, map) => {
  let cls = Term.UTyp.cls_of_term(term);
  let ty = utyp_to_ty(ctx, utyp);
  /* let add = (ctx, m, id) => Id.Map.add(id, InfoTyp({cls, ctx, ty}), m); */
  let add = (~ctx, ~self, ~free, m) => (
    typ_after_fix(ctx, mode, self),
    free,
    Id.Map.add(id, InfoTyp({cls, mode, self, ctx, ty, free}), m),
  );
  let atomic = (ctx, self) => add(~ctx, ~self, ~free=[], Id.Map.empty);
  /* let return = (ctx, m) => (ty, add(~ctx, m, id)); */
  switch (term) {
  | Invalid(msg, _) => (
      Typ.unknown(Internal),
      [],
      Id.Map.singleton(id, Invalid(msg)),
    )
  | EmptyHole => atomic(ctx, Just(Typ.unknown(SynSwitch)))
  | Int => atomic(ctx, Just(Typ.int()))
  | Float => atomic(ctx, Just(Typ.float()))
  | Bool => atomic(ctx, Just(Typ.bool()))
  | Var(name) =>
    switch (Typ.Ctx.tyvar_named(ctx, name)) {
    | None => atomic(ctx, Free)
    | Some((_, tyvar_entry)) =>
      add(
        ~ctx,
        ~self=Just(Typ.Kind.to_typ(tyvar_entry.kind)),
        ~free=[(name, [{id, mode}])],
        Id.Map.empty,
      )
    }

  | List(t)
  | Parens(t) =>
    let (typ, free, m) = utyp_to_info_map(~ctx, ~mode, t);
    add(~ctx, ~self=Just(typ), ~free, m);
  | Arrow(t1, t2) =>
    let (typ1, free1, m_t1) = utyp_to_info_map(~ctx, t1);
    let (typ2, free2, m_t2) = utyp_to_info_map(~ctx, t2);
    add(
      ~ctx,
      ~self=Just(Typ.arrow(typ1, typ2)),
      ~free=free1 @ free2,
      union_m([m_t1, m_t2]),
    );
  | MultiHole(ids, ts)
  | Tuple(ids, ts) =>
    let results = List.map(utyp_to_info_map(~ctx), ts);
    /* |> List.fold_left( */
    /*      ((typs_rev, free, m), (typ, free1, m1)) => */
    /*        ([typ, ...typs_rev], free1 @ free, union_m([m1, m])), */
    /*      ([], [], Id.Map.empty), */
    /*   ); */
    let m =
      List.fold_left(
        (m, (id, (typ, free, m1))) =>
        {let (_, _, m) = add(~ctx, ~self=Just(typ), ~free, id, m);
         m
      },
        m,
        List.combine(ids, results),
      );
    (ty, free, m);
  };
}
and utpat_to_info_map =
    (~ctx=Typ.Ctx.empty(), ~mode: Typ.mode=Typ.Syn, {id, term}: Term.UTPat.t)
    : (Typ.Kind.t, Typ.Ctx.t, map) => {
  let cls = Term.UTPat.cls_of_term(term);
  let add = (~self, ~ctx, m) => (
    Typ.Kind.singleton(typ_after_fix(ctx, mode, self)),
    ctx,
    Id.Map.add(id, InfoTPat({cls, self, mode, ctx}), m),
  );
  let atomic = self => add(~self, ~ctx, Id.Map.empty);
  switch (term) {
  | Invalid(msg, _) => (
      Typ.Kind.unknown(),
      ctx,
      Id.Map.singleton(id, Invalid(msg)),
    )
  | EmptyHole => atomic(Just(Typ.unknown(SynSwitch)))
  | Var(name) =>
    let self = Typ.Just(Typ.unknown(SynSwitch));
    let kind =
      switch (mode) {
      | Syn => Typ.Kind.unknown()
      | Ana(typ) => Typ.Kind.singleton(typ)
      };
    let ctx = Typ.Ctx.add_tyvar(ctx, {id, name, kind});
    add(~self, ~ctx, Id.Map.empty);
  };
};

let mk_map =
  Core_kernel.Memo.general(
    ~cache_size_bound=1000,
    uexp_to_info_map(~ctx=Typ.Ctx.empty()),
  );
