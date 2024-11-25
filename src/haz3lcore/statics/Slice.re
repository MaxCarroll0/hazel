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

[@deriving (show({with_path: false}), sexp, yojson)]
type slice = (Ctx.t, list(Id.t)); //TODO: Represent slices in a more efficient (non-list) way

[@deriving (show({with_path: false}), sexp, yojson)]
type s_ty =
  | TEMP // TO BE REMOVED: placeholder
  | Unknown // These will have empty slices
  | SynSwitch // These may have non-empty slices -- the synthesised slices of the corresponding term
  | Int
  | Float
  | Bool
  | String
  | Var(string) // TODO: Type vars are complicated -- see Typ weak-head-normalisation. Will require passing ctx around
  | List(t) // Note, t is a join here
  | Arrow(t, t)
  | Sum(ConstructorMap.t(t))
  | Prod(list(t))
  | Ap(t, t)
  | Join(Ctx.t, list(t)) // Keep components of join types around
  | Rec(TermBase.tpat_t, t)
  | Forall(TermBase.tpat_t, t)
and t = (Typ.t, s_ty, slice); // For efficiency, keep Typ.t around (allowing fast ty_of without revaluating joins)

let empty: slice = ([], []);
let temp = (ty): t => (ty, TEMP, empty);

let ty_of: t => Typ.t = ((ty, _, _)) => ty;

// TODO: filter duplicates
let union2: (slice, slice) => slice =
  ((c1, s1), (c2, s2)) => (c1 @ c2, s1 @ s2);
let union: list(slice) => slice = List.fold_left(union2, empty);

let append: (slice, t) => t =
  (s', (ty, s_ty, s)) => (ty, s_ty, union2(s, s'));

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

let hole = (Unknown(Internal) |> Typ.temp, Unknown, empty);
let hole_with_ids = ids => (
  Unknown(Internal) |> Typ.temp,
  Unknown,
  of_ids(ids),
);
let synswitch = (s): t => (Unknown(SynSwitch) |> Typ.temp, SynSwitch, s);

let tag_ty = ((_, s_ty, s): t, ty): t => (ty, s_ty, s);

let matched_arrow_strict = (ctx: Ctx.t, (ty, s_ty, s)) =>
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

let matched_arrow = (ctx: Ctx.t, s: t) =>
  matched_arrow_strict(ctx, s) |> Option.value(~default=(hole, hole));

let matched_prod_strict = (ctx: Ctx.t, length, (ty, s_ty, s)) =>
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

let matched_prod = (ctx: Ctx.t, length, s: t) =>
  matched_prod_strict(ctx, length, s)
  |> Option.value(~default=List.init(length, _ => hole));

let matched_list_strict = (ctx: Ctx.t, (ty, s_ty, s)) =>
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

let matched_list = (ctx: Ctx.t, s: t) =>
  matched_list_strict(ctx, s) |> Option.value(~default=hole);

let matched_args = (ctx, default_arity, (ty, s_ty, s)) => {
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
