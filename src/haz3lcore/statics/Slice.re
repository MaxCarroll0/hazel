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

 */

[@deriving (show({with_path: false}), sexp, yojson)]
type slice = (Ctx.t, list(Id.t)); //TODO: Represent slices in a more efficient (non-list) way

[@deriving (show({with_path: false}), sexp, yojson)]
type typ =
  | TEMP // For unknown stuff
  | Unknown // Could add extra cases for unknown types of different provenances and error status etc.
  | Int
  | Float
  | Bool
  | String
  | Var(string) // TODO: is Var name needed?
  | List(t) // Note, t is a join here
  | Arrow(t, t)
  | Sum(ConstructorMap.t(t))
  | Prod(list(t))
  | Ap(t, t)
  | Join(list(t)) // Keep components of join types around
  | Rec(TermBase.tpat_t, t) // TODO: are type patterns needed?
  | Forall(TermBase.tpat_t, t)
and t = (typ, slice);

let empty: slice = ([], []);
// TODO: filter duplicates
let union2: (slice, slice) => slice =
  ((c1, s1), (c2, s2)) => (c1 @ c2, s1 @ s2);
let union: list(slice) => slice = List.fold_left(union2, empty);

let join = (l: list(t), slice): t => (Join(l), slice);

let rec full_slice = (ctx: Ctx.t, (ty, s): t): slice => {
  let calc' = full_slice(ctx);
  switch (ty) {
  | TEMP =>
    print_endline("Temp full slice");
    empty;
  | Unknown
  | Int
  | Float
  | Bool
  | String
  | Var(_) => s
  | List(t1)
  | Rec(_, t1)
  | Forall(_, t1) => union2(s, calc'(t1))
  | Arrow(t1, t2)
  | Ap(t1, t2) => union([s, calc'(t1), calc'(t2)])
  | Sum(m) =>
    ConstructorMap.(
      let f = (ss, v) =>
        switch (v) {
        | Variant(_, _, Some((_, s)))
        | BadEntry((_, s)) => [s, ...ss]
        | Variant(_, _, None) => ss
        };
      union([s, ...List.fold_left(f, [], m)]);
    )
  | Prod(ts)
  | Join(ts) => union([s, ...List.map(calc', ts)])
  };
};

let of_ids = (ids: list(Id.t)): slice => ([], ids);

let of_ctx = (ctx: Ctx.t): slice => (ctx, []);

// All lists contain joins with an empty slice. Debatable if this is true/useful. Encode in types if so.
let extract_join_list = (t: t): (list(t), slice) =>
  switch (t) {
  | (List((Join(ss), ([], []))), s) => (ss, s)
  | _ =>
    print_endline("List slice has join with non-empty slice");
    ([], empty); // TODO: Check + what to do here
  };

let typ = ((typ, _): t): typ => typ;

let slice = ((_, slice): t): slice => slice;

let rec ty_of_typ = (ctx: Ctx.t, typ: typ) =>
  TermBase.(
    switch (typ) {
    | TEMP =>
      print_endline("ty of temp slice");
      Unknown(Internal);
    | Unknown => Unknown(Internal)
    | Int => Int
    | Float => Float
    | Bool => Bool
    | String => String
    | Var(string) => Var(string)
    | List(t) => List(ty_of(ctx, t))
    | Arrow(t1, t2) => Arrow(ty_of(ctx, t1), ty_of(ctx, t2))
    | Sum(m) => Sum(ConstructorMap.map_vals(ty_of(ctx), m))
    | Prod(l) => Prod(List.map(ty_of(ctx), l))
    | Ap(t1, t2) => Ap(ty_of(ctx, t1), ty_of(ctx, t2))
    | Join(l) =>
      Option.value(
        ~default=Unknown(Internal) |> UTyp.temp,
        UTyp.join_all(
          ctx,
          List.map(ty_of(ctx), l),
          ~empty=Unknown(Internal) |> UTyp.temp,
        ),
      )
      |> UTyp.term_of // TODO: Check
    | Rec(pat, t) => Rec(pat, ty_of(ctx, t))
    | Forall(pat, t) => Forall(pat, ty_of(ctx, t))
    }
  )
  |> Typ.temp
and ty_of = (ctx: Ctx.t, t: t): TermBase.typ_t => ty_of_typ(ctx, typ(t));

// Tags type with slice. Recursively tags subcomponents of the type with same slices
// Very inefficient!!
let rec of_ty = (s: slice, t: TermBase.typ_t) => (
  switch (Typ.term_of(t)) {
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
  | Parens(ty) => typ(of_ty(s, ty))
  | Rec(pat, ty) => Rec(pat, of_ty(s, ty))
  | Forall(pat, ty) => Forall(pat, of_ty(s, ty))
  },
  s,
);

let temp = (TEMP, empty);
