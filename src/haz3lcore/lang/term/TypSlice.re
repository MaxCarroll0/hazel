open Util;
open OptUtil.Syntax;

[@deriving (show({with_path: false}), sexp, yojson)]
type cls_typ = Typ.cls;
[@deriving (show({with_path: false}), sexp, yojson)]
type cls_slc =
  | Typ
  | SliceIncr
  | SliceGlobal;
[@deriving (show({with_path: false}), sexp, yojson)]
type cls = (cls_slc, cls_typ);

include TermBase.TypSlice;

let term_of: t => term = IdTagged.term_of;
let unwrap: t => (term, term => t) = IdTagged.unwrap;

let fresh: term => t = IdTagged.fresh;
/* fresh assigns a random id, whereas temp assigns Id.invalid, which
   is a lot faster, and since we so often make types and throw them away
   shortly after, it makes sense to use it. */
let temp: term => t = term => {term, ids: [Id.invalid], copied: false};
let rep_id: t => Id.t = IdTagged.rep_id;

let all_ids_temp = {
  let f:
    'a.
    (IdTagged.t('a) => IdTagged.t('a), IdTagged.t('a)) => IdTagged.t('a)
   =
    (continue, exp) => {...exp, ids: [Id.invalid]} |> continue;
  map_term(~f_exp=f, ~f_pat=f, ~f_typ=f, ~f_tpat=f, ~f_rul=f);
};

let (replace_temp, replace_temp_exp) = {
  let f:
    'a.
    (IdTagged.t('a) => IdTagged.t('a), IdTagged.t('a)) => IdTagged.t('a)
   =
    (continue, exp) =>
      {...exp, ids: exp.ids == [Id.invalid] ? [Id.mk()] : exp.ids}
      |> continue;
  (
    map_term(~f_exp=f, ~f_pat=f, ~f_typ=f, ~f_tpat=f, ~f_rul=f),
    TermBase.Exp.map_term(~f_exp=f, ~f_pat=f, ~f_typ=f, ~f_tpat=f, ~f_rul=f),
  );
};

let empty_code: code = {ctx_used: [], term_ids: []};

let slice_of_ids = (term_ids): code => {ctx_used: [], term_ids};
let slice_of_ctx = (ctx_used: list(ctx_var)): code => {
  ctx_used,
  term_ids: [],
};
let slice_of_ctx_ids = (ctx_used, term_ids): code => {ctx_used, term_ids};

let union_code: (code, code) => code =
  ({ctx_used: ctx1, term_ids: c1}, {ctx_used: ctx2, term_ids: c2}) => {
    ctx_used: ctx1 @ ctx2,
    term_ids: c1 @ c2,
  };

let get_incr_slice: term => option(code) =
  fun
  | `Typ(_)
  | `Global(_) => None
  | `Incr(_, code) => Some(code);
let get_global_slice: term => option(code) =
  fun
  | `Typ(_)
  | `Incr(_) => None
  | `Global(_, code) => Some(code);
let get_incr_slice_or_empty = s =>
  s |> get_incr_slice |> Option.value(~default=empty_code);
let get_global_slice_or_empty = s =>
  s |> get_global_slice |> Option.value(~default=empty_code);

// wraps a code slice inside a `Global: unioning the global slices if required
let wrap_global = wrap_global;
// wraps a code slice inside an incremental slice (unioning if required)
let wrap_incr = wrap_incr;

// Fold over the code segments. TODO: helpers that separate folding over global/incremental slices
let rec fold =
        (
          f: ('acc, code) => 'acc,
          u: ('acc, 'acc) => 'acc,
          init: 'acc,
          s: term,
        )
        : 'acc => {
  switch (s) {
  | `Typ(ty) =>
    switch (ty) {
    | Unknown(_)
    | Int
    | Float
    | Bool
    | String
    | Var(_)
    | Sum(_) => init
    | List(s)
    | Parens(s)
    | Rec(_, s)
    | Forall(_, s) => term_of(s) |> fold(f, u, init)
    | Arrow(s1, s2)
    | Ap(s1, s2) =>
      u(fold(f, u, init, term_of(s1)), fold(f, u, init, term_of(s2)))
    | Prod(ss) =>
      ss
      |> List.fold_left(
           (acc, s) => u(fold(f, u, init, term_of(s)), acc),
           init,
         )
    }
  | `Incr(s, code) => f(fold(f, u, init, (s :> term)), code)
  | `Global(s, code) => f(fold(f, u, init, (s :> term)), code)
  };
};

// Applies f : typ => 'a' onto the Type part of a slice: discards existing slices
let apply = (f, s: term): 'a => {
  let apply_typ = (`Typ(ty): typ_term) => f(ty);
  let apply_incr = (s: incr_term) =>
    switch (s) {
    | `Typ(_) as s
    | `Incr(s, _) => apply_typ(s)
    };
  switch (s) {
  | `Typ(_) as s
  | `Incr(_) as s
  | `Global(s, _) => apply_incr(s)
  };
};
let apply_t: (typ => term, t) => t = f => IdTagged.apply(apply(f));

// Applies f : typ => typ onto the type part of a slice: preserving existing slices
let apply_typ = (f, s: term): term => {
  let apply_typ_term = (`Typ(ty): typ_term) => `Typ(f(ty));
  let apply_incr_term = (s: incr_term) =>
    switch (s) {
    | `Typ(_) as s => apply_typ_term(s)
    | `Incr(s, code) => `Incr((apply_typ_term(s), code))
    };
  switch (s) {
  | `Typ(_) as s
  | `Incr(_) as s => apply_incr_term(s)
  | `Global(s, code) => `Global((apply_incr_term(s), code))
  };
};
let apply_typ_t: (typ => typ, t) => t = f => IdTagged.apply(apply_typ(f));

let slice_typ_of_term = apply(x => x);
let slice_typ_of = IdTagged.apply(slice_typ_of_term);

// Produces a slice of the same structure as the type, matching slices where the structure is identical
let match_slices = failwith("TODO");

// Apply a function f : typ => term, merging with the slices on the typ
let rec apply_merge = (f, s: term): term => {
  let apply_typ_term = (`Typ(ty): typ_term): term => f(ty);
  let apply_incr_term = (s: incr_term): term =>
    switch (s) {
    | `Typ(_) as s => s |> apply_typ_term
    | `Incr(s, code) => s |> apply_typ_term |> wrap_incr(code)
    };
  switch (s) {
  | `Typ(_) as s
  | `Incr(_) as s => s |> apply_incr_term
  | `Global(s, code) => s |> apply_incr_term |> wrap_global(code)
  };
};
let apply_merge_t: (typ => term, t) => t =
  f => IdTagged.apply(apply_merge(f));

let hole = (tms: list(TermBase.Any.t)): TermBase.Typ.term =>
  switch (tms) {
  | [] => Unknown(Hole(EmptyHole))
  | [_, ..._] => Unknown(Hole(MultiHole(tms)))
  };

let cls_slc_of_term: term => cls_slc =
  fun
  | `Typ(_) => Typ
  | `Incr(_) => SliceIncr
  | `Global(_) => SliceGlobal;

let cls_typ_of_term: term => cls_typ =
  s => s |> typ_term_of_term |> Typ.cls_of_term;

let show_cls_typ: cls_typ => string = Typ.show_cls;

let cls_slc_of_term: term => cls_slc =
  s =>
    switch (s) {
    | `Typ(_) => Typ
    | `Incr(_) => SliceIncr
    | `Global(_) => SliceGlobal
    };

let show_cls_slc: cls_slc => string =
  cls =>
    switch (cls) {
    | Typ => "Typ"
    | SliceIncr => "SliceIncr"
    | SliceGlobal => "SliceGlobal"
    };

let cls_of_term = (s): cls => (cls_slc_of_term(s), cls_typ_of_term(s));
let show_cls = ((cls_slc, cls_typ): cls) =>
  show_cls_slc(cls_slc) ++ "(" ++ show_cls_typ(cls_typ) ++ ")";

//TODO: remove duplicates
let rec full_slice: term => code = fold(union_code, union_code, empty_code);

// These pattern matching functions can be optimised by direct pattern matching vs use of typ_of
// TODO: Implement directly for efficiency purposes
let is_unknown = (~ignore_parens=?, s: t) =>
  s |> typ_of |> Typ.is_unknown(~ignore_parens?);
let is_arrow = (~ignore_parens=?, s: t) =>
  s |> typ_of |> Typ.is_arrow(~ignore_parens?);
let is_parens = (s: t) => s |> typ_of |> Typ.is_parens;

let is_forall = (~ignore_parens=?, s: t) =>
  s |> typ_of |> Typ.is_forall(~ignore_parens?);

let is_list = (~ignore_parens=?, s: t) =>
  s |> typ_of |> Typ.is_list(~ignore_parens?);
let is_sum = (~ignore_parens=?, s: t) =>
  s |> typ_of |> Typ.is_sum(~ignore_parens?);

[@deriving (show({with_path: false}), sexp, yojson)]
type source = {
  id: Id.t,
  ty: t,
};

/* Strip location information from a list of sources */
let of_source = List.map((source: source) => source.ty);

let join_type_provenance = Typ.join_type_provenance;

let rec free_vars = s => s |> typ_of |> Typ.free_vars;

let var_count = Typ.var_count;
let fresh_var = Typ.fresh_var;

let unroll = 
  apply_merge_t(ty => {
  switch (ty) {
  | Rec(tp, s) => subst(`Typ(ty), tp, s) |> term_of
  | _ => ty
  }})

/*
   TypSlice equality: Extending type equality to slices.
   This is type equality, different slices of the same type ARE equal. Type Equality: This coincides with alpha equivalence for normalized types.
 Other types may be equivalent but this will not detect so if they are not normalized. */
let eq = (t1: t, t2: t): bool => fast_equal(t1, t2);

let typslc_typ_t_of_typ = (ty: Typ.t): typslc_typ_t => {
  let (ty, rewrap) = ty |> IdTagged.unwrap;
  (Typ(ty): typslc_typ_term) |> rewrap;
};

let typslc_typ_t_of_slc = (s: slc_typ_t): typslc_typ_t => {
  let (s, rewrap) = s |> IdTagged.unwrap;
  (Slice(s): typslc_typ_term) |> rewrap;
};

let t_of_typ_t = (ty: Typ.t): t => {
  let (ty, rewrap) = ty |> IdTagged.unwrap;
  `Typ(ty) |> rewrap;
};

// Creates a slice from the ids of a typ_t. Used in creating slices of type annotations.
// This could instead be done directly in parsing.
let rec t_of_typ_t_sliced = ({ids, _} as ty: Typ.t): t => {
  let (ty, rewrap) = ty |> IdTagged.unwrap;
  (
    switch (ty) {
    | Unknown(_) => (`Typ(ty): term) // Don't slice holes
    | Int => `SliceIncr((Typ(Int), slice_of_ids(ids)))
    | Bool => `SliceIncr((Typ(Bool), slice_of_ids(ids)))
    | Float => `SliceIncr((Typ(Float), slice_of_ids(ids)))
    | String => `SliceIncr((Typ(String), slice_of_ids(ids)))
    | Var(name) => `SliceIncr((Typ(Var(name)), slice_of_ids(ids)))
    // Note: ctx slice not relevant in the above (types used in local scope)
    | List(t) =>
      `SliceIncr((Slice(List(t_of_typ_t_sliced(t))), slice_of_ids(ids)))
    | Parens(t) =>
      `SliceIncr((Slice(Parens(t_of_typ_t_sliced(t))), slice_of_ids(ids)))
    | Arrow(t1, t2) =>
      `SliceIncr((
        Slice(Arrow(t_of_typ_t_sliced(t1), t_of_typ_t_sliced(t2))),
        slice_of_ids(ids),
      ))
    | Ap(t1, t2) =>
      `SliceIncr((
        Slice(Ap(t_of_typ_t_sliced(t1), t_of_typ_t_sliced(t2))),
        slice_of_ids(ids),
      ))
    | Sum(m) =>
      `SliceIncr((
        Slice(Sum(m |> ConstructorMap.map_vals(t_of_typ_t_sliced))),
        slice_of_ids(ids),
      ))
    | Prod(ts) =>
      `SliceIncr((
        Slice(Prod(List.map(t_of_typ_t_sliced, ts))),
        slice_of_ids(ids),
      ))
    | Rec(tpat, t) =>
      `SliceIncr((
        Slice(Rec(tpat, t_of_typ_t_sliced(t))),
        slice_of_ids(ids),
      ))
    | Forall(tpat, t) =>
      `SliceIncr((
        Slice(Forall(tpat, t_of_typ_t_sliced(t))),
        slice_of_ids(ids),
      ))
    }
  )
  |> rewrap;
};

// Same as above but making global slices
let rec t_of_typ_t_sliced_global = ({ids, _} as ty: Typ.t): t => {
  let (ty, rewrap) = ty |> IdTagged.unwrap;
  (
    switch (ty) {
    | Unknown(_) => (`Typ(ty): term) // Don't slice holes
    | Int => `SliceGlobal((`Typ(Int), slice_of_ids(ids)))
    | Bool => `SliceGlobal((`Typ(Bool), slice_of_ids(ids)))
    | Float => `SliceGlobal((`Typ(Float), slice_of_ids(ids)))
    | String => `SliceGlobal((`Typ(String), slice_of_ids(ids)))
    | Var(name) => `SliceGlobal((`Typ(Var(name)), slice_of_ids(ids)))
    // Note: ctx slice not relevant in the above (types used in local scope)
    | List(t) =>
      `SliceGlobal((Slice(List(t_of_typ_t_sliced(t))), slice_of_ids(ids)))
    | Parens(t) =>
      `SliceGlobal((
        Slice(Parens(t_of_typ_t_sliced(t))),
        slice_of_ids(ids),
      ))
    | Arrow(t1, t2) =>
      `SliceGlobal((
        Slice(Arrow(t_of_typ_t_sliced(t1), t_of_typ_t_sliced(t2))),
        slice_of_ids(ids),
      ))
    | Ap(t1, t2) =>
      `SliceIncr((
        Slice(Ap(t_of_typ_t_sliced(t1), t_of_typ_t_sliced(t2))),
        slice_of_ids(ids),
      ))
    | Sum(m) =>
      `SliceIncr((
        Slice(Sum(m |> ConstructorMap.map_vals(t_of_typ_t_sliced))),
        slice_of_ids(ids),
      ))
    | Prod(ts) =>
      `SliceIncr((
        Slice(Prod(List.map(t_of_typ_t_sliced, ts))),
        slice_of_ids(ids),
      ))
    | Rec(tpat, t) =>
      `SliceIncr((
        Slice(Rec(tpat, t_of_typ_t_sliced(t))),
        slice_of_ids(ids),
      ))
    | Forall(tpat, t) =>
      `SliceIncr((
        Slice(Forall(tpat, t_of_typ_t_sliced(t))),
        slice_of_ids(ids),
      ))
    }
  )
  |> rewrap;
};

let t_of_slc_typ_t = (ty: slc_typ_t): t => {
  let (ty, rewrap) = ty |> IdTagged.unwrap;
  `SliceIncr((Slice(ty): typslc_typ_term, empty_slice_incr)) |> rewrap;
};

let rec typslc_typ_term_of_term = (s: term): typslc_typ_term =>
  switch (s) {
  | `Typ(ty) => Typ(ty)
  | `SliceIncr(s, _) => s
  | `SliceGlobal(s, _) => typslc_typ_term_of_term((s :> term))
  };

let rec term_of_slc_typ_term = (s: slc_typ_term): term =>
  `SliceIncr((Slice(s), empty_slice_incr));

/* Lattice join on slices. This is a LUB join in the hazel2
   sense in that any type dominates Unknown. The optional
   resolve parameter specifies whether, in the case of a type
   variable and a succesful join, to return the resolved join type,
   or to return the (first) type variable for readability */
/* Slices are joined as follows:
      The leaves of the two type slices are matched and joined as above.
      When considering a slice, if any of it's leaves are used in the
      final join, then all incremental and/or global slices must be used.
      Strictly use the left slice when given a choice.

      TODO: Proof that this computes a minimal code slice which synthesises
      the same join type.
   */
let rec join_using =
        (
          ~resolve=false,
          ~fix,
          ctx: Ctx.t,
          {term: term1, _} as s1: t,
          {term: term2, _} as s2: t,
        )
        : option((t, BranchUsed.t)) => {
  let join' = join_using(~resolve, ~fix, ctx);
  let rewrap1 = term' => {...s1, term: term'};
  let rewrap2 = term' => {...s2, term: term'};
  open BranchUsed;
  let join_typ_rewrap = (f, ty1, ty2) =>
    Typ.join_using(~resolve, ~fix, ctx, ty1 |> rewrap1, ty2 |> rewrap2)
    |> Option.map(((ty, b)) => {
         let (ty', rewrap') = ty |> IdTagged.unwrap;
         (ty', b) |> f |> (((ty', b)) => (rewrap'(ty'), b));
       });
  let join_typ_rewrap_idbranch = f =>
    join_typ_rewrap(((a, b)) => (f(a), b));

  let choose_branch = (branch_used, slice_incr1, slice_incr2) =>
    left(branch_used)
      ? slice_incr1 : right(branch_used) ? slice_incr2 : empty_slice_incr;

  switch (term1, term2) {
  | (`Typ(ty1), `Typ(ty2)) =>
    join_typ_rewrap_idbranch(ty => `Typ(ty), ty1, ty2)
  | (`SliceIncr(Typ(ty1), slice_incr), `Typ(ty2)) =>
    join_typ_rewrap(
      ((ty, b)): (term, BranchUsed.t) =>
        (left(b) ? `SliceIncr((Typ(ty), slice_incr)) : `Typ(ty), b),
      ty1,
      ty2,
    )
  | (`Typ(ty1), `SliceIncr(Typ(ty2), slice_incr)) =>
    join_typ_rewrap(
      ((ty, b)): (term, BranchUsed.t) =>
        (right(b) ? `SliceIncr((Typ(ty), slice_incr)) : `Typ(ty), b),
      ty1,
      ty2,
    )
  | (
      `SliceIncr(Slice(s1'), slice_incr1),
      `SliceIncr(Slice(s2'), slice_incr2),
    ) =>
    //TODO: remove redundancy here with the above case... somehow?
    switch (s1', s2') {
    | (_, Parens(s2)) => join'(s1, s2)
    | (Parens(s1), _) => join'(s1, s2)
    /* Note: Ordering of Unknown, Var, and Rec above is load-bearing! */
    | (Rec(tp1, s1), Rec(tp2, s2)) =>
      let ctx = Ctx.extend_dummy_tvar(ctx, tp1);
      let s1' =
        switch (TPat.tyvar_of_utpat(tp2)) {
        | Some(x2) => subst(`Typ(Var(x2)) |> temp, tp1, s1)
        | None => s1
        };
      let+ (s_body, branch_used) = join_using(~resolve, ~fix, ctx, s1', s2);
      (
        `SliceIncr((
          Slice(Rec(tp1, s_body)),
          choose_branch(branch_used, slice_incr1, slice_incr2),
        ))
        |> temp,
        branch_used,
      );
    | (Rec(_), _) => None
    | (Forall(x1, s1), Forall(x2, s2)) =>
      let ctx = Ctx.extend_dummy_tvar(ctx, x1);
      let ty1' =
        switch (TPat.tyvar_of_utpat(x2)) {
        | Some(x2) => subst(`Typ(Var(x2)) |> temp, x1, s1)
        | None => s1
        };
      let+ (s_body, branch_used) = join_using(~resolve, ~fix, ctx, ty1', s2);
      (
        `SliceIncr((
          Slice(Forall(x1, s_body)),
          choose_branch(branch_used, slice_incr1, slice_incr2),
        ))
        |> temp,
        branch_used,
      );
    /* Note for above: there is no danger of free variable capture as
       subst itself performs capture avoiding substitution. However this
       may generate internal type variable names that in corner cases can
       be exposed to the user. We preserve the variable name of the
       second type to preserve synthesized type variable names, which
       come from user annotations. */
    | (Forall(_), _) => None
    | (Arrow(s1, s2), Arrow(s1', s2')) =>
      let* (s1, branch_used1) = join'(s1, s1');
      let+ (s2, branch_used2) = join'(s2, s2');
      let branch_used = combine_branches_used(branch_used1, branch_used2);
      (
        `SliceIncr((
          Slice(Arrow(s1, s2)),
          choose_branch(branch_used, slice_incr1, slice_incr2),
        ))
        |> temp,
        branch_used,
      );
    | (Arrow(_), _) => None
    | (Prod(ss1), Prod(ss2)) =>
      let* joins = ListUtil.map2_opt(join', ss1, ss2);
      let+ joins = OptUtil.sequence(joins);
      let (ss, branches_used) = ListUtil.unzip(joins);
      let branch_used =
        List.fold_left(combine_branches_used, None, branches_used);
      (
        `SliceIncr((
          Slice(Prod(ss)),
          choose_branch(branch_used, slice_incr1, slice_incr2),
        ))
        |> temp,
        branch_used,
      );
    | (Prod(_), _) => None
    | (Sum(sm1), Sum(sm2)) =>
      let+ (sm', branches_used) =
        ConstructorMap.join_using(
          eq,
          join_using(~resolve, ~fix, ctx),
          sm1,
          sm2,
        );
      let branch_used =
        List.fold_left(combine_branches_used, None, branches_used);
      (
        `SliceIncr((
          Slice(Sum(sm')),
          choose_branch(branch_used, slice_incr1, slice_incr2),
        ))
        |> temp,
        branch_used,
      ); // TODO: Check!
    | (Sum(_), _) => None
    | (List(s1), List(s2)) =>
      let+ (s, branch_used) = join'(s1, s2);
      (
        `SliceIncr((
          Slice(List(s)),
          choose_branch(branch_used, slice_incr1, slice_incr2),
        ))
        |> temp,
        branch_used,
      );
    | (List(_), _) => None
    | (Ap(_), _) => failwith("Type join of ap")
    }
  // TODO: remove redundancy here somehow?
  | (`SliceIncr(Slice(s1'), slice_incr), `Typ(ty2)) =>
    switch (s1', ty2) {
    | (_, Parens(ty2)) => join'(s1, ty2 |> t_of_typ_t)
    | (Parens(s1), _) => join'(s1, s2)
    | (_, Unknown(Hole(_))) when fix =>
      /* NOTE(andrew): This is load bearing
         for ensuring that function literals get appropriate
         casts. Documentation/Dynamics has regression tests */
      Some((s2, Right)) // TODO: Check this rule
    | (_, Unknown(Internal | SynSwitch)) => Some((s1, Left))
    | (_, Var(name)) =>
      let* s_name = Ctx.lookup_alias(ctx, name);
      let+ (s_join, branch_used) = join'(s_name, s1);
      !resolve && eq(s_name, s_join) ? (s2, Right) : (s_join, branch_used);
    /* Note: Ordering of Unknown, Var, and Rec above is load-bearing! */
    | (Rec(tp1, s1), Rec(tp2, ty2)) =>
      let ctx = Ctx.extend_dummy_tvar(ctx, tp1);
      let s1' =
        switch (TPat.tyvar_of_utpat(tp2)) {
        | Some(x2) => subst(`Typ(Var(x2)) |> temp, tp1, s1)
        | None => s1
        };
      let+ (s_body, branch_used) =
        join_using(~resolve, ~fix, ctx, s1', ty2 |> t_of_typ_t);
      (
        `SliceIncr((
          Slice(Rec(tp1, s_body)),
          choose_branch(branch_used, slice_incr, empty_slice_incr),
        ))
        |> temp,
        branch_used,
      );
    | (Rec(_), _) => None
    | (Forall(x1, s1), Forall(x2, ty2)) =>
      let ctx = Ctx.extend_dummy_tvar(ctx, x1);
      let s1' =
        switch (TPat.tyvar_of_utpat(x2)) {
        | Some(x2) => subst(`Typ(Var(x2)) |> temp, x1, s1)
        | None => s1
        };
      let+ (s_body, branch_used) =
        join_using(~resolve, ~fix, ctx, s1', ty2 |> t_of_typ_t);
      (
        `SliceIncr((
          Slice(Forall(x1, s_body)),
          choose_branch(branch_used, slice_incr, empty_slice_incr),
        ))
        |> temp,
        branch_used,
      );
    /* Note for above: there is no danger of free variable capture as
       subst itself performs capture avoiding substitution. However this
       may generate internal type variable names that in corner cases can
       be exposed to the user. We preserve the variable name of the
       second type to preserve synthesized type variable names, which
       come from user annotations. */
    | (Forall(_), _) => None
    | (Arrow(s1, s2), Arrow(ty1', ty2')) =>
      let* (s1, branch_used1) = join'(s1, ty1' |> t_of_typ_t);
      let+ (s2, branch_used2) = join'(s2, ty2' |> t_of_typ_t);
      let branch_used = combine_branches_used(branch_used1, branch_used2);
      (
        `SliceIncr((
          Slice(Arrow(s1, s2)),
          choose_branch(branch_used, slice_incr, empty_slice_incr),
        ))
        |> temp,
        branch_used,
      );
    | (Arrow(_), _) => None
    | (Prod(ss1), Prod(tys2)) =>
      let* joins = ListUtil.map2_opt(join', ss1, List.map(t_of_typ_t, tys2));
      let+ joins = OptUtil.sequence(joins);
      let (ss, branches_used) = ListUtil.unzip(joins);
      let branch_used =
        List.fold_left(combine_branches_used, None, branches_used);
      (
        `SliceIncr((
          Slice(Prod(ss)),
          choose_branch(branch_used, slice_incr, empty_slice_incr),
        ))
        |> temp,
        branch_used,
      );
    | (Prod(_), _) => None
    | (Sum(sm1), Sum(sm2)) =>
      let sm2 = ConstructorMap.map_vals(t_of_typ_t, sm2);
      let+ (sm', branches_used) =
        ConstructorMap.join_using(
          eq,
          join_using(~resolve, ~fix, ctx),
          sm1,
          sm2,
        );
      let branch_used =
        List.fold_left(combine_branches_used, None, branches_used);
      (
        `SliceIncr((
          Slice(Sum(sm')),
          choose_branch(branch_used, slice_incr, empty_slice_incr),
        ))
        |> temp,
        branch_used,
      ); // TODO: Check!
    | (Sum(_), _) => None
    | (List(s1), List(ty2)) =>
      let+ (s, branch_used) = join'(s1, ty2 |> t_of_typ_t);
      (
        `SliceIncr((
          Slice(List(s)),
          choose_branch(branch_used, slice_incr, empty_slice_incr),
        ))
        |> temp,
        branch_used,
      );
    | (List(_), _) => None
    | (Ap(_), _) => failwith("Type join of ap")
    }
  // This case could be implemented via flipping like `SliceGlobal below. But the 'fix' rule must be done managed correctly
  // For simplicity I have just copied the rules. The redundancy between these 3 cases should be minimised somehow ideally...
  | (`Typ(ty1), `SliceIncr(Slice(s2'), slice_incr2)) =>
    switch (ty1, s2') {
    | (_, Parens(s2)) => join'(s1, s2)
    | (Parens(ty1), _) => join'(ty1 |> t_of_typ_t, s2)
    | (Unknown(_), _) => Some((s2, Right))
    | (Var(name), _) =>
      let* s_name = Ctx.lookup_alias(ctx, name);
      let+ (s_join, branch_used) = join'(s_name, s2);
      !resolve && eq(s_name, s_join) ? (s1, Left) : (s_join, branch_used);
    /* Note: Ordering of Unknown, Var, and Rec above is load-bearing! */
    | (Rec(tp1, ty1), Rec(tp2, s2)) =>
      let ctx = Ctx.extend_dummy_tvar(ctx, tp1);
      let s1' =
        switch (TPat.tyvar_of_utpat(tp2)) {
        | Some(x2) => subst(`Typ(Var(x2)) |> temp, tp1, ty1 |> t_of_typ_t)
        | None => s1
        };
      let+ (s_body, branch_used) = join_using(~resolve, ~fix, ctx, s1', s2);
      (
        `SliceIncr((
          Slice(Rec(tp1, s_body)),
          choose_branch(branch_used, empty_slice_incr, slice_incr2),
        ))
        |> temp,
        branch_used,
      );
    | (Rec(_), _) => None
    | (Forall(x1, ty1), Forall(x2, s2)) =>
      let ctx = Ctx.extend_dummy_tvar(ctx, x1);
      let s1' =
        switch (TPat.tyvar_of_utpat(x2)) {
        | Some(x2) => subst(`Typ(Var(x2)) |> temp, x1, ty1 |> t_of_typ_t)
        | None => s1
        };
      let+ (s_body, branch_used) = join_using(~resolve, ~fix, ctx, s1', s2);
      (
        `SliceIncr((
          Slice(Forall(x1, s_body)),
          choose_branch(branch_used, empty_slice_incr, slice_incr2),
        ))
        |> temp,
        branch_used,
      );
    /* Note for above: there is no danger of free variable capture as
       subst itself performs capture avoiding substitution. However this
       may generate internal type variable names that in corner cases can
       be exposed to the user. We preserve the variable name of the
       second type to preserve synthesized type variable names, which
       come from user annotations. */
    | (Forall(_), _) => None
    | (Int, _) => None
    | (Float, _) => None
    | (Bool, _) => None
    | (String, _) => None
    | (Arrow(ty1, ty2), Arrow(s1', s2')) =>
      let* (s1, branch_used1) = join'(ty1 |> t_of_typ_t, s1');
      let+ (s2, branch_used2) = join'(ty2 |> t_of_typ_t, s2');
      let branch_used = combine_branches_used(branch_used1, branch_used2);
      (
        `SliceIncr((
          Slice(Arrow(s1, s2)),
          choose_branch(branch_used, empty_slice_incr, slice_incr2),
        ))
        |> temp,
        branch_used,
      );
    | (Arrow(_), _) => None
    | (Prod(tys1), Prod(ss2)) =>
      let* joins = ListUtil.map2_opt(join', List.map(t_of_typ_t, tys1), ss2);
      let+ joins = OptUtil.sequence(joins);
      let (ss, branches_used) = ListUtil.unzip(joins);
      let branch_used =
        List.fold_left(combine_branches_used, None, branches_used);
      (
        `SliceIncr((
          Slice(Prod(ss)),
          choose_branch(branch_used, empty_slice_incr, slice_incr2),
        ))
        |> temp,
        branch_used,
      );
    | (Prod(_), _) => None
    | (Sum(sm1), Sum(sm2)) =>
      let sm1 = ConstructorMap.map_vals(t_of_typ_t, sm1);
      let+ (sm', branches_used) =
        ConstructorMap.join_using(
          eq,
          join_using(~resolve, ~fix, ctx),
          sm1,
          sm2,
        );
      let branch_used =
        List.fold_left(combine_branches_used, None, branches_used);
      (
        `SliceIncr((
          Slice(Sum(sm')),
          choose_branch(branch_used, empty_slice_incr, slice_incr2),
        ))
        |> temp,
        branch_used,
      ); // TODO: Check!
    | (Sum(_), _) => None
    | (List(ty1), List(s2)) =>
      let+ (s, branch_used) = join'(ty1 |> t_of_typ_t, s2);
      (
        `SliceIncr((
          Slice(List(s)),
          choose_branch(branch_used, empty_slice_incr, slice_incr2),
        ))
        |> temp,
        branch_used,
      );
    | (List(_), _) => None
    | (Ap(_), _) => failwith("Type join of ap")
    }
  | (`SliceIncr(Typ(ty1), slice_incr1), _) =>
    let+ (s, branch_used) = join'(`Typ(ty1) |> rewrap1, s2);
    (left(branch_used) ? wrap_incr(slice_incr1, s) : s, branch_used);
  | (_, `SliceIncr(Typ(ty2), slice_incr2)) =>
    let+ (s, branch_used) = join'(s1, `Typ(ty2) |> rewrap2);
    (right(branch_used) ? wrap_incr(slice_incr2, s) : s, branch_used);
  | (`SliceGlobal(s1, slice_global1), _) =>
    let+ (s, branch_used) = join'((s1 :> term) |> rewrap1, s2);
    (left(branch_used) ? wrap_global(slice_global1, s) : s, branch_used);
  | (_, `SliceGlobal(s2, slice_global2)) =>
    let+ (s, branch_used) = join'(s1, (s2 :> term) |> rewrap2);
    (right(branch_used) ? wrap_global(slice_global2, s) : s, branch_used);
  };
};

let rec join = (~resolve=false, ~fix, ctx: Ctx.t, ty1: t, ty2: t): option(t) =>
  join_using(~resolve, ~fix, ctx, ty1, ty2) |> Option.map(fst);

/* REQUIRES NORMALIZED TYPES
   Remove synswitches from t1 by matching against t2 */
let rec match_synswitch =
        ({term: term1, _} as s1: t, {term: term2, _} as s2: t): t => {
  let rewrap1 = term' => {...s1, term: term'};
  let rewrap2 = term' => {...s2, term: term'};
  switch (term1, term2) {
  | (`Typ(ty1), _) =>
    `Typ(Typ.match_synswitch(ty1 |> rewrap1, s2 |> typ_of) |> Typ.term_of)
    |> rewrap1
  | (`SliceIncr(Typ(ty1), slice_incr), _) =>
    (
      `SliceIncr((
        Typ(
          Typ.match_synswitch(ty1 |> rewrap1, s2 |> typ_of) |> Typ.term_of,
        ),
        slice_incr,
      )): term
    )
    |> rewrap1
  | (`SliceIncr(Slice(s1'), slice_incr), `Typ(ty2))
  | (`SliceIncr(Slice(s1'), slice_incr), `SliceIncr(Typ(ty2), _)) =>
    switch (s1', ty2) {
    | (Parens(s1), _) =>
      (
        `SliceIncr((
          Slice(Parens(match_synswitch(s1, `Typ(ty2) |> rewrap2))),
          slice_incr,
        )): term
      )
      |> rewrap1
    // These cases can't have a synswitch inside
    | (Ap(_), _)
    | (Rec(_), _)
    | (Forall(_), _) => s1
    // These might
    | (List(s1), List(ty2)) =>
      (
        `SliceIncr((
          Slice(List(match_synswitch(s1, ty2 |> t_of_typ_t))),
          slice_incr,
        )): term
      )
      |> rewrap1
    | (List(_), _) => s1
    | (Arrow(s1, s2), Arrow(ty1', ty2')) =>
      (
        `SliceIncr((
          Slice(
            Arrow(
              match_synswitch(s1, ty1' |> t_of_typ_t),
              match_synswitch(s2, ty2' |> t_of_typ_t),
            ),
          ),
          slice_incr,
        )): term
      )
      |> rewrap1
    | (Arrow(_), _) => s1
    | (Prod(ss1), Prod(tys2)) when List.length(ss1) == List.length(tys2) =>
      let ss = List.map2(match_synswitch, ss1, List.map(t_of_typ_t, tys2));
      (`SliceIncr((Slice(Prod(ss)), slice_incr)): term) |> rewrap1;
    | (Prod(_), _) => s1
    | (Sum(sm1), Sum(sm2)) =>
      let sm2 = ConstructorMap.map_vals(t_of_typ_t, sm2);
      let sm' = ConstructorMap.match_synswitch(match_synswitch, eq, sm1, sm2);
      (`SliceIncr((Slice(Sum(sm')), slice_incr)): term) |> rewrap1;
    | (Sum(_), _) => s1
    }
  | (`SliceIncr(Slice(s1'), slice_incr1), `SliceIncr(Slice(s2'), _)) =>
    switch (s1', s2') {
    | (Parens(s1), _) =>
      (
        `SliceIncr((Slice(Parens(match_synswitch(s1, s2))), slice_incr1)): term
      )
      |> rewrap1
    // These cases can't have a synswitch inside
    | (Ap(_), _)
    | (Rec(_), _)
    | (Forall(_), _) => s1
    // These might
    | (List(s1), List(s2)) =>
      (
        `SliceIncr((Slice(List(match_synswitch(s1, s2))), slice_incr1)): term
      )
      |> rewrap1
    | (List(_), _) => s1
    | (Arrow(s1, s2), Arrow(s1', s2')) =>
      (
        `SliceIncr((
          Slice(Arrow(match_synswitch(s1, s1'), match_synswitch(s2, s2'))),
          slice_incr1,
        )): term
      )
      |> rewrap1
    | (Arrow(_), _) => s1
    | (Prod(ss1), Prod(ss2)) when List.length(ss1) == List.length(ss2) =>
      let ss = List.map2(match_synswitch, ss1, ss2);
      (`SliceIncr((Slice(Prod(ss)), slice_incr1)): term) |> rewrap1;
    | (Prod(_), _) => s1
    | (Sum(sm1), Sum(sm2)) =>
      let sm' = ConstructorMap.match_synswitch(match_synswitch, eq, sm1, sm2);
      (`SliceIncr((Slice(Sum(sm')), slice_incr1)): term) |> rewrap1;
    | (Sum(_), _) => s1
    }
  | (`SliceGlobal(s1', slice_global), _) =>
    match_synswitch((s1' :> term) |> rewrap1, s2)
    |> wrap_global(slice_global)
  | (_, `SliceGlobal(s2', slice_global)) =>
    match_synswitch(s1, (s2' :> term) |> rewrap2)
    |> wrap_global(slice_global)
  };
};

let join_fix = join(~fix=true);

let join_all = (~empty: t, ctx: Ctx.t, ts: list(t)): option(t) =>
  List.fold_left(
    (acc, ty) => OptUtil.and_then(join(~fix=false, ctx, ty), acc),
    Some(empty),
    ts,
  );

let is_consistent = (ctx: Ctx.t, ty1: t, ty2: t): bool =>
  join(~fix=false, ctx, ty1, ty2) != None;

// Destructuring constructs
let unparens =
  map_merge(
    fun
    | Parens(ty) => t_of_typ_t(ty)
    | _ => failwith("Not a parens"),
    fun
    | Parens(s) => s
    | _ => failwith("Not a parens"),
  );

// Normalisation
let rec weak_head_normalize = (ctx: Ctx.t, s: t): t => {
  let (_, rewrap) = s |> IdTagged.unwrap;
  let typ_normalize = (ty: Typ.term) =>
    `Typ(Typ.weak_head_normalize(ctx, ty |> Typ.fresh) |> Typ.term_of)
    |> rewrap;
  let slc_normalize = (ty: slc_typ_term) =>
    switch (ty) {
    | Parens(s') => weak_head_normalize(ctx, s')
    | _ => s
    };
  map_merge(typ_normalize, slc_normalize, s);
};

let wrap_empty_incr = s =>
  `SliceIncr((Slice(s): typslc_typ_term, empty_slice_incr));

let rec normalize = (ctx: Ctx.t, s: t): t => {
  let (_, rewrap) = s |> IdTagged.unwrap;
  let typ_normalize = (ty: Typ.term) =>
    `Typ(Typ.normalize(ctx, ty |> Typ.fresh) |> Typ.term_of) |> rewrap;
  let slc_normalize = (ty: slc_typ_term) => {
    switch (ty) {
    | Parens(t) => Parens(normalize(ctx, t)) |> wrap_empty_incr |> rewrap
    | List(t) => List(normalize(ctx, t)) |> wrap_empty_incr |> rewrap
    | Ap(t1, t2) =>
      Ap(normalize(ctx, t1), normalize(ctx, t2))
      |> wrap_empty_incr
      |> rewrap
    | Arrow(t1, t2) =>
      Arrow(normalize(ctx, t1), normalize(ctx, t2))
      |> wrap_empty_incr
      |> rewrap
    | Prod(ts) =>
      Prod(List.map(normalize(ctx), ts)) |> wrap_empty_incr |> rewrap
    | Sum(ts) =>
      Sum(ConstructorMap.map(Option.map(normalize(ctx)), ts))
      |> wrap_empty_incr
      |> rewrap
    | Rec(tpat, ty) =>
      /* NOTE: Dummy tvar added has fake id but shouldn't matter
         as in current implementation Recs do not occur in the
         surface syntax, so we won't try to jump to them. */
      Rec(tpat, normalize(Ctx.extend_dummy_tvar(ctx, tpat), ty))
      |> wrap_empty_incr
      |> rewrap
    | Forall(name, ty) =>
      Forall(name, normalize(Ctx.extend_dummy_tvar(ctx, name), ty))
      |> wrap_empty_incr
      |> rewrap
    };
  };
  map_merge(typ_normalize, slc_normalize, s);
};

// Matching functions
let rec matched_arrow_strict = (ctx, s: t) => {
  let (_, rewrap) = s |> IdTagged.unwrap;
  switch (term_of(weak_head_normalize(ctx, s))) {
  | `Typ(ty)
  | `SliceIncr(Typ(ty), _) =>
    Typ.matched_arrow_strict(ctx, ty |> rewrap)
    |> Option.map(TupleUtil.map2(t_of_typ_t))
  | `SliceIncr(Slice(s'), _) =>
    switch (s') {
    | Parens(_) => matched_arrow_strict(ctx, unparens(s))
    | Arrow(s1, s2) => Some((s1, s2))
    | _ => None
    }
  | `SliceGlobal(s, slice_global) =>
    matched_arrow_strict(ctx, (s :> term) |> fresh)
    |> Option.map(TupleUtil.map2(wrap_global(slice_global)))
  };
};

let matched_arrow = (ctx, ty) =>
  matched_arrow_strict(ctx, ty)
  |> Option.value(
       ~default=(
         `Typ(Unknown(Internal)) |> temp,
         `Typ(Unknown(Internal)) |> temp,
       ),
     );

let rec matched_forall_strict = (ctx, s) => {
  let (_, rewrap) = s |> IdTagged.unwrap;
  switch (term_of(weak_head_normalize(ctx, s))) {
  | `Typ(ty)
  | `SliceIncr(Typ(ty), _) =>
    Typ.matched_forall_strict(ctx, ty |> rewrap)
    |> Option.map(((tpat, ty)) => (tpat, t_of_typ_t(ty)))
  | `SliceIncr(Slice(s'), _) =>
    switch (s') {
    | Parens(_) => matched_forall_strict(ctx, unparens(s))
    | Forall(t, ty) => Some((Some(t), ty))
    | _ => None // (None, Unknown(Internal) |> temp)
    }
  | `SliceGlobal(s, slice_global) =>
    matched_forall_strict(ctx, (s :> term) |> fresh)
    |> Option.map(((tpat, s)) => (tpat, wrap_global(slice_global, s)))
  };
};

let matched_forall = (ctx, ty) =>
  matched_forall_strict(ctx, ty)
  |> Option.value(~default=(None, `Typ(Unknown(Internal)) |> temp));

let rec matched_prod_strict = (ctx, length, s) => {
  let (_, rewrap) = s |> IdTagged.unwrap;
  switch (term_of(weak_head_normalize(ctx, s))) {
  | `Typ(ty)
  | `SliceIncr(Typ(ty), _) =>
    Typ.matched_prod_strict(ctx, length, ty |> rewrap)
    |> Option.map(List.map(t_of_typ_t))
  | `SliceIncr(Slice(s'), _) =>
    switch (s') {
    | Parens(_) => matched_prod_strict(ctx, length, unparens(s))
    | Prod(tys) when List.length(tys) == length => Some(tys)
    | _ => None
    }
  | `SliceGlobal(s, slice_global) =>
    matched_prod_strict(ctx, length, (s :> term) |> fresh)
    |> Option.map(List.map(wrap_global(slice_global)))
  };
};

let matched_prod = (ctx, length, ty) =>
  matched_prod_strict(ctx, length, ty)
  |> Option.value(
       ~default=List.init(length, _ => `Typ(Unknown(Internal)) |> temp),
     );

let rec matched_list_strict = (ctx, s) => {
  let (_, rewrap) = s |> IdTagged.unwrap;
  switch (term_of(weak_head_normalize(ctx, s))) {
  | `Typ(ty)
  | `SliceIncr(Typ(ty), _) =>
    Typ.matched_list_strict(ctx, ty |> rewrap) |> Option.map(t_of_typ_t)
  | `SliceIncr(Slice(s'), _) =>
    switch (s') {
    | Parens(_) => matched_list_strict(ctx, unparens(s))
    | List(ty) => Some(ty)
    | _ => None
    }
  | `SliceGlobal(s, slice_global) =>
    matched_list_strict(ctx, (s :> term) |> fresh)
    |> Option.map(wrap_global(slice_global))
  };
};

let matched_list = (ctx, ty) =>
  matched_list_strict(ctx, ty)
  |> Option.value(~default=`Typ(Unknown(Internal)) |> temp);

let rec matched_args = (ctx, default_arity, s) => {
  let (_, rewrap) = s |> IdTagged.unwrap;
  let s' = weak_head_normalize(ctx, s);
  switch (term_of(s')) {
  | `Typ(ty)
  | `SliceIncr(Typ(ty), _) =>
    Typ.matched_args(ctx, default_arity, ty |> rewrap)
    |> List.map(t_of_typ_t)
  | `SliceIncr(Slice(s''), _) =>
    switch (s'') {
    | Parens(_) => matched_args(ctx, default_arity, unparens(s))
    | Prod([_, ..._] as tys) => tys
    | _ => [s']
    }
  | `SliceGlobal(s, slice_global) =>
    matched_args(ctx, default_arity, (s :> term) |> fresh)
    |> List.map(wrap_global(slice_global))
  };
};

let rec get_sum_constructors =
        (ctx: Ctx.t, {term, _} as s: t): option(sum_map) => {
  let rewrap = term' => {...s, term: term'};
  let s = weak_head_normalize(ctx, s);
  switch (term) {
  | `Typ(ty)
  | `SliceIncr(Typ(ty), _) =>
    Typ.get_sum_constructors(ctx, ty |> rewrap)
    |> Option.map(ConstructorMap.map_vals(t_of_typ_t))
  | `SliceIncr(Slice(s'), _) =>
    switch (s') {
    | Parens(_) => get_sum_constructors(ctx, unparens(s))
    | Sum(sm) => Some(sm)
    | Rec(_) =>
      /* Note: We must unroll here to get right ctr types;
         otherwise the rec parameter will leak. However, seeing
         as substitution is too expensive to be used here, we
         currently making the optimization that, since all
         recursive types are type alises which use the alias name
         as the recursive parameter, and type aliases cannot be
         shadowed, it is safe to simply remove the Rec constructor,
         provided we haven't escaped the context in which the alias
         is bound. If either of the above assumptions become invalid,
         the below code will be incorrect! */
      let unr = ({term, _}: incr_t) =>
        switch (term) {
        | `Typ(Rec({term: Var(x), _}, ty_body))
        | `SliceIncr(Typ(Rec({term: Var(x), _}, ty_body)), _) =>
          switch (Ctx.lookup_alias(ctx, x)) {
          | None => unroll(s)
          | Some(_) =>
            let (term, rewrap) = ty_body |> IdTagged.unwrap;
            `Typ(term) |> rewrap;
          }
        | `SliceIncr(Slice(Rec({term: Var(x), _}, s_body)), _) =>
          switch (Ctx.lookup_alias(ctx, x)) {
          | None => unroll(s)
          | Some(_) => s_body
          }
        | _ => s
        };
      let s =
        switch (s |> term_of) {
        | `SliceGlobal(s, _) => unr(s |> rewrap)
        | `Typ(_) as s
        | `SliceIncr(_) as s => unr(s |> rewrap)
        };
      switch (s |> term_of) {
      | `Typ(Sum(sm))
      | `SliceIncr(Typ(Sum(sm)), _)
      | `SliceGlobal(`Typ(Sum(sm)), _)
      | `SliceGlobal(`SliceIncr(Typ(Sum(sm)), _), _) =>
        Some(ConstructorMap.map_vals(t_of_typ_t, sm))
      | `SliceIncr(Slice(Sum(sm)), _)
      | `SliceGlobal(`SliceIncr(Slice(Sum(sm)), _), _) => Some(sm)
      | #term => None
      };
    | _ => None
    }
  | _ => None
  };
};

let is_synswitch = s =>
  s
  |> term_of
  |> apply(
       fun
       | Unknown(SynSwitch) => true
       | _ => false,
       _ => false);

/* Does the type require parentheses when on the left of an arrow for printing? */
let rec needs_parens = (s: t): bool => Typ.needs_parens(typ_of(s));

let pretty_print_tvar = (tv: TPat.t): string => Typ.pretty_print_tvar(tv);

/* Essentially recreates haz3lweb/view/Type.re's view_ty but with string output */
let rec pretty_print = (s: t): string => Typ.pretty_print(typ_of(s))

and ctr_pretty_print =
  fun
  | ConstructorMap.Variant(ctr, _, None) => ctr
  | ConstructorMap.Variant(ctr, _, Some(t)) =>
    ctr ++ "(" ++ pretty_print(t) ++ ")"
  | ConstructorMap.BadEntry(_) => "?"
and paren_pretty_print = typ =>
  if (needs_parens(typ)) {
    "(" ++ pretty_print(typ) ++ ")";
  } else {
    pretty_print(typ);
  };
