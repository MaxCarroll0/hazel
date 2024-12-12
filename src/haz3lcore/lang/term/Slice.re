open Util;

/*

     TYPE SLICES:
     Type slices are list of IDs that contribute to the given type.
     With each subcomponent of the type having it's own slice, so that
     types can be desconstructed giving the desconstructed type slices.

     Slices are represented incrementally, with each base type containing
     it's slice, and all compound types adding only the extra Id's involved
     in e.g. producing a function type (the 'fun _ -> _' construct)Typ

     A flattened slice can be constructed with full_slice. This contains
     only the slice of code contributing the the final type.

    Interaction Design:
    - Use cursor inspector to get a type: which is either an analytic or synthesised type
    - Click the type to get: an analysis type provenance (see MODE), or a type slice of synthetic type

 */

include TermBase.Slice;

let empty: slice = ([], []);
let temp = (ty): t => (ty, TEMP, empty);

let ty_of: t => Typ.t = ((ty, _, _)) => ty;
let rec ty_of_slow: t => Typ.t =
  ((_, s_ty, _) as s) =>
    (
      switch (s_ty) {
      | TEMP
      | Unknown => Unknown(Internal)
      | SynSwitch => Unknown(SynSwitch)
      | Int => Int
      | Float => Float
      | Bool => Bool
      | String => String
      | Var(x) => Var(x)
      | List(t1) => List(ty_of_slow(t1))
      | Rec(tpat, t1) => Rec(tpat, ty_of_slow(t1))
      | Forall(tpat, t1) => Forall(tpat, ty_of_slow(t1))
      | Arrow(t1, t2) => Arrow(ty_of_slow(t1), ty_of_slow(t2))
      | Ap(t1, t2) => Ap(ty_of_slow(t1), ty_of_slow(t2))
      | Prod(ts) => Prod(List.map(ty_of_slow, ts))
      | Sum(_)
      | Join(_) => ty_of(s) |> Typ.term_of
      }
    )
    |> Typ.temp;

// TODO: filter duplicates
let union2: (slice, slice) => slice =
  ((c1, s1), (c2, s2)) => (c1 @ c2, s1 @ s2);
let union: list(slice) => slice = List.fold_left(union2, empty);

let append: (slice, t) => t =
  (s', (ty, s_ty, s)) => (ty, s_ty, union2(s, s'));

// TODO: create a slice type which applies has a 'global part' using less space than the below.
let rec append_all: (slice, t) => t =
  (s', (ty, s_ty, s)) => (
    ty,
    switch (s_ty) {
    | TEMP
    | Unknown
    | SynSwitch
    | Int
    | Float
    | Bool
    | String
    | Var(_) => s_ty
    | List(ty) => List(append_all(s', ty))
    | Arrow(ty1, ty2) => Arrow(append_all(s', ty1), append_all(s', ty2))
    | Sum(m) => Sum(ConstructorMap.map_vals(append_all(s'), m))
    | Prod(ts) => Prod(List.map(append_all(s'), ts))
    | Join(ctx, ts) => Join(ctx, List.map(append_all(s'), ts))
    | Ap(t1, t2) => Ap(append_all(s', t1), append_all(s', t2))
    | Rec(pat, t) => Rec(pat, append_all(s', t))
    | Forall(pat, t) => Forall(pat, append_all(s', t))
    },
    union2(s, s'),
  );

let join = (ctx, ty: Typ.t, l: list(t), slice): t => (
  ty,
  Join(ctx, l),
  slice,
);

let rec full_slice = ((_, s_ty, s): t): slice => {
  switch (s_ty) {
  | TEMP =>
    print_endline("Temp full slice");
    empty;
  | SynSwitch
  | Unknown
  | Int
  | Float
  | Bool
  | String
  | Var(_) => s
  | List(t1)
  | Rec(_, t1)
  | Forall(_, t1) => union2(s, full_slice(t1))
  | Arrow(t1, t2)
  | Ap(t1, t2) => union([s, full_slice(t1), full_slice(t2)])
  | Sum(m) =>
    ConstructorMap.(
      let f = (ss, v) =>
        switch (v) {
        | Variant(_, _, Some((_, _, s)))
        | BadEntry((_, _, s)) => [s, ...ss]
        | Variant(_, _, None) => ss
        };
      union([s, ...List.fold_left(f, [], m)]);
    )
  | Prod(ts)
  | Join(_, ts) => union([s, ...List.map(full_slice, ts)]) // TODO: Ignore irrelevant slices in join
  };
};

let of_ids = (ids: list(Id.t)): slice => ([], ids);

let of_ctx = (ctx: Ctx.t): slice => (ctx, []);

let of_ctx_ids = (ctx, ids): slice => (ctx, ids);

let s_ty = ((_, s_ty, _): t): s_ty => s_ty;

let slice = ((_, _, slice): t): slice => slice;

// Tags type with slice. Recursively tags subcomponents of the type with same slices
let rec of_ty = (s: slice, ty: TermBase.typ_t): t => (
  ty,
  switch (Typ.term_of(ty)) {
  | Unknown(_) => Unknown
  | Int => Int
  | Float => Float
  | Bool => Bool
  | String => String
  | Var(n) => Var(n)
  | List(ty) => List(of_ty(s, ty))
  | Arrow(ty1, ty2) => Arrow(of_ty(s, ty1), of_ty(s, ty2))
  | Sum(m) => Sum(ConstructorMap.map_vals(of_ty(s), m))
  | Prod(l) => Prod(List.map(of_ty(s), l))
  | Ap(ty1, ty2) => Ap(of_ty(s, ty1), of_ty(s, ty2))
  | Parens(ty) => s_ty(of_ty(s, ty))
  | Rec(pat, ty) => Rec(pat, of_ty(s, ty))
  | Forall(pat, ty) => Forall(pat, of_ty(s, ty))
  },
  s,
);

// Use the ids of the type as the slices directly.
// Only useful for type originating from the program code. e.g. annotations
let rec of_ty_with_ids = ({term, ids, _} as ty: TermBase.typ_t): t => (
  ty,
  switch (term) {
  | Unknown(_) => Unknown
  | Int => Int
  | Float => Float
  | Bool => Bool
  | String => String
  | Var(n) => Var(n)
  | List(ty) => List(of_ty_with_ids(ty) |> append(of_ids(ids)))
  // Keep structural elements in subslices (e.g. ->). May be sensible to put this logic elsewhere
  | Arrow(ty1, ty2) =>
    Arrow(
      of_ty_with_ids(ty1) |> append(of_ids(ids)),
      of_ty_with_ids(ty2) |> append(of_ids(ids)),
    )
  | Sum(m) =>
    Sum(
      ConstructorMap.map_vals(
        ty => of_ty_with_ids(ty) |> append(of_ids(ids)),
        m,
      ),
    )
  | Prod(l) =>
    Prod(List.map(ty => of_ty_with_ids(ty) |> append(of_ids(ids)), l))
  | Ap(ty1, ty2) =>
    Ap(
      of_ty_with_ids(ty1) |> append(of_ids(ids)),
      of_ty_with_ids(ty2) |> append(of_ids(ids)),
    )
  | Parens(ty) => s_ty(of_ty_with_ids(ty))
  | Rec(pat, ty) => Rec(pat, of_ty_with_ids(ty) |> append(of_ids(ids)))
  | Forall(pat, ty) =>
    Forall(pat, of_ty_with_ids(ty) |> append(of_ids(ids)))
  },
  of_ids(ids),
);

let hole: t = (Unknown(Internal) |> Typ.temp, Unknown, empty);
let hole_with_ids: list(Id.t) => t =
  ids => (Unknown(Internal) |> Typ.temp, Unknown, of_ids(ids));
let synswitch = (s): t => (Unknown(SynSwitch) |> Typ.temp, SynSwitch, s);

let tag_ty = ((_, s_ty, s): t, ty): t => (ty, s_ty, s);

let matched_arrow_strict = (ctx: Ctx.t, (ty, s_ty, s): t): option((t, t)) =>
  Option.bind(
    switch (s_ty) {
    | Arrow(s_in, s_out) => Some((s_in, s_out))
    | SynSwitch => Some((synswitch(s), synswitch(s)))
    | TEMP =>
      Option.map(
        TupleUtil.map2(tag_ty(temp(Unknown(Internal) |> Typ.temp))),
        Typ.matched_arrow_strict(ctx, ty),
      )
    | _ => None
    },
    ((s1, s2)) =>
    Option.map(
      ((ty1, ty2)) => (tag_ty(s1, ty1), tag_ty(s2, ty2)),
      Typ.matched_arrow_strict(ctx, ty),
    )
  );

let matched_arrow = (ctx: Ctx.t, s: t): (t, t) =>
  matched_arrow_strict(ctx, s) |> Option.value(~default=(hole, hole));

let matched_prod_strict =
    (ctx: Ctx.t, length, (ty, s_ty, s): t): option(list(t)) =>
  Option.bind(
    switch (s_ty) {
    | Prod(ss) when List.length(ss) == length => Some(ss)
    | SynSwitch => Some(List.init(length, _ => synswitch(s)))
    | TEMP =>
      Option.map(
        List.map(tag_ty(temp(Unknown(Internal) |> Typ.temp))),
        Typ.matched_prod_strict(ctx, length, ty),
      )
    | _ => None
    },
    l_s =>
    Option.map(
      List.map2(tag_ty, l_s),
      Typ.matched_prod_strict(ctx, length, ty),
    )
  );

let matched_prod = (ctx: Ctx.t, length, s: t): list(t) =>
  matched_prod_strict(ctx, length, s)
  |> Option.value(~default=List.init(length, _ => hole));

let matched_list_strict = (ctx: Ctx.t, (ty, s_ty, s): t): option(t) =>
  Option.bind(
    switch (s_ty) {
    | List(s) => Some(s)
    | SynSwitch => Some(synswitch(s))
    | TEMP =>
      Option.map(
        tag_ty(temp(Unknown(Internal) |> Typ.temp)),
        Typ.matched_list_strict(ctx, ty),
      )
    | _ => None
    },
    s =>
    Option.map(tag_ty(s), Typ.matched_list_strict(ctx, ty))
  );

let matched_list = (ctx: Ctx.t, s: t): t =>
  matched_list_strict(ctx, s) |> Option.value(~default=hole);

let matched_args = (ctx, default_arity, (ty, s_ty, s): t): list(t) => {
  List.map2(
    tag_ty,
    switch (s_ty) {
    | Prod([_, ..._] as ss) => ss
    | Unknown
    | SynSwitch => List.init(default_arity, _ => (ty, s_ty, s))
    | TEMP =>
      List.map(
        tag_ty(temp(Unknown(Internal) |> Typ.temp)),
        Typ.matched_args(ctx, default_arity, ty),
      )
    | _ => [(ty, s_ty, s)]
    },
    Typ.matched_args(ctx, default_arity, ty),
  );
};

/* REQUIRES NORMALIZED TYPES
   Remove synswitches from s1 by matching against s2 */
let rec match_synswitch = ((t1, s_ty1, c1): t, (t2, s_ty2, c2): t) => {
  (
    Typ.match_synswitch(t1, t2),
    switch (s_ty1, s_ty2) {
    | (SynSwitch, _) => s_ty2
    // These cases can't have a synswitch inside
    | (Unknown, _)
    | (Int, _)
    | (Float, _)
    | (Bool, _)
    | (String, _)
    | (Var(_), _)
    | (Ap(_), _)
    | (Rec(_), _)
    | (Forall(_), _)
    | (TEMP, _) => s_ty1
    // These might
    | (List(s1), List(s2)) => List(match_synswitch(s1, s2))
    | (List(_), _) => s_ty1
    | (Arrow(s1, s2), Arrow(s1', s2')) =>
      Arrow(match_synswitch(s1, s1'), match_synswitch(s2, s2'))
    | (Arrow(_), _) => s_ty1
    | (Prod(ss1), Prod(ss2)) when List.length(ss1) == List.length(ss2) =>
      let ss = List.map2(match_synswitch, ss1, ss2);
      Prod(ss);
    | (Prod(_), _) => s_ty1
    | (Join(_, ss1), Join(ctx2, ss2))
        when List.length(ss1) == List.length(ss2) =>
      let ss = List.map2(match_synswitch, ss1, ss2);
      Join(ctx2, ss); // No harm in extending the context
    | (Join(_), _) => s_ty1
    | (Sum(sm1), Sum(sm2)) =>
      // (Require normalisation for fast_equal)
      let sm' =
        ConstructorMap.match_synswitch(
          match_synswitch,
          fast_equal_tys,
          sm1,
          sm2,
        );
      Sum(sm');
    | (Sum(_), _) => s_ty1
    },
    switch (s_ty1) {
    | SynSwitch => c2
    | _ => c1
    },
  );
};
