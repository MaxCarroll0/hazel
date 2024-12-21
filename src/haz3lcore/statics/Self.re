open Util;

/* SELF.re

   This module defines the SELF data structure, which represents
   the synthetic type information derivable from a term independent
   of the type expectation (i.e. MODE) of its syntactic context. This
   synethetic information is not entirely independent, in that it still
   uses the typing context passed down from the syntactic context.

   A term which from which a type can be derived in isolation, that is,
   that has a valid synthetic typing judgement, will generally have a SELF
   of Just(some_type). (The one current exception are the constructors of labelled
   sum types, which are handled specially as their synthetic type
   may be 'overwritten' by the analytic expectation)

   The other cases all represent states for which no single type can be
   derived, such as syntactic errors, or branching constructs which may
   have inconsistent types.

   */

[@deriving (show({with_path: false}), sexp, yojson)]
type join_type =
  | Id
  | List;

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Just(TypSlice.t) /* Just a regular type */
  | NoJoin(join_type, list(TypSlice.source)) /* Inconsistent types for e.g match, listlits */
  | BadToken(Token.t) /* Invalid expression token, treated as hole */
  | BadTrivAp(TypSlice.t) /* Trivial (nullary) ap on function that doesn't take triv */
  | IsMulti /* Multihole, treated as hole */
  | IsConstructor({
      name: Constructor.t,
      syn_ty: option(TypSlice.t),
    }); /* Constructors have special ana logic */

[@deriving (show({with_path: false}), sexp, yojson)]
type error_partial_ap =
  | NoDeferredArgs
  | ArityMismatch({
      expected: int,
      actual: int,
    });

/* Expressions can also be free variables */
[@deriving (show({with_path: false}), sexp, yojson)]
type exp =
  | Free(Var.t)
  | InexhaustiveMatch(exp)
  | IsDeferral(Exp.deferral_position)
  | IsBadPartialAp(error_partial_ap)
  | Common(t);

[@deriving (show({with_path: false}), sexp, yojson)]
type pat =
  | Redundant(pat)
  | Common(t);

let join_of = (j: join_type, ty: TypSlice.t): TypSlice.t =>
  switch (j) {
  | Id => ty
  | List =>
    `SliceIncr((Slice(List(ty)), TypSlice.empty_slice_incr))
    |> TypSlice.fresh
  };

/* What the type would be if the position had been
   synthetic, so no hole fixing. Returns none if
   there's no applicable synthetic rule. */
let typ_of: (Ctx.t, t) => option(TypSlice.t) =
  _ctx =>
    fun
    | Just(typ) => Some(typ)
    | IsConstructor({syn_ty, _}) => syn_ty
    | BadToken(_)
    | BadTrivAp(_)
    | IsMulti
    | NoJoin(_) => None;

let typ_of_exp: (Ctx.t, exp) => option(TypSlice.t) =
  ctx =>
    fun
    | Free(_)
    | InexhaustiveMatch(_)
    | IsDeferral(_)
    | IsBadPartialAp(_) => None
    | Common(self) => typ_of(ctx, self);

let rec typ_of_pat: (Ctx.t, pat) => option(TypSlice.t) =
  ctx =>
    fun
    | Redundant(pat) => typ_of_pat(ctx, pat)
    | Common(self) => typ_of(ctx, self);

/* The self of a var depends on the ctx; if the
   lookup fails, it is a free variable */
// Slice of a var is the ids of the term + the var_entry in ctx_used + the slice from the context
let of_exp_var = (ids: list(Id.t), ctx: Ctx.t, name: Var.t): exp =>
  switch (Ctx.lookup_var(ctx, name)) {
  | None => Free(name)
  | Some(var) =>
    Common(
      Just(
        TypSlice.(
          var.typ |> wrap_incr(slice_of_ctx_ids([Var(var.name)], ids))
        ),
      ),
    )
  };

/* The self of a ctr depends on the ctx, but a
   lookup failure doesn't necessarily means its
   free; it may be given a type analytically */
// The syn slice should include ids of the ctr
let of_ctr = (ids, ctx: Ctx.t, name: Constructor.t): t =>
  IsConstructor({
    name,
    syn_ty:
      switch (Ctx.lookup_ctr(ctx, name)) {
      | None => None
      | Some({typ, _}) =>
        Some(
          typ |> TypSlice.(wrap_incr(slice_of_ctx_ids([Ctr(name)], ids))),
        )
      },
  });

let of_deferred_ap =
    (args, ty_ins: list(TypSlice.t), ty_out: TypSlice.t): exp => {
  let expected = List.length(ty_ins);
  let actual = List.length(args);
  if (expected != actual) {
    IsBadPartialAp(ArityMismatch({expected, actual}));
  } else if (List.for_all(Exp.is_deferral, args)) {
    IsBadPartialAp(NoDeferredArgs);
  } else {
    let ty_ins =
      List.combine(args, ty_ins)
      |> List.filter(((arg, _ty)) => Exp.is_deferral(arg))
      |> List.map(snd);
    let ty_in =
      List.length(ty_ins) == 1
        ? List.hd(ty_ins)
        : `SliceIncr((Slice(Prod(ty_ins)), TypSlice.empty_slice_incr))
          |> TypSlice.fresh;
    Common(
      Just(
        `SliceIncr((Slice(Arrow(ty_in, ty_out)), TypSlice.empty_slice_incr))
        |> TypSlice.fresh,
      ),
    );
  };
};

let add_source = List.map2((id, ty) => TypSlice.{id, ty});

let of_match =
    (ids: list(Id.t), ctx: Ctx.t, tys: list(TypSlice.t), c_ids: list(Id.t))
    : t =>
  switch (
    TypSlice.join_all(
      ~empty=`Typ(Unknown(Internal)) |> TypSlice.fresh,
      ctx,
      tys,
    )
  ) {
  | None => NoJoin(Id, add_source(c_ids, tys))
  | Some(ty) => Just(ty |> TypSlice.(wrap_incr(slice_of_ids(ids))))
  };

// Slices of list literals is just the join of the element slices + the ids of the listlit constructor
let of_list_lit =
    (
      ~empty,
      ids: list(Id.t),
      ctx: Ctx.t,
      tys: list(TypSlice.t),
      elem_ids: list(Id.t),
    )
    : t =>
  switch (TypSlice.join_all(~empty, ctx, tys)) {
  | None => NoJoin(List, add_source(elem_ids, tys))
  | Some(ty) =>
    Just(
      `SliceIncr((Slice(List(ty)), TypSlice.slice_of_ids(ids)))
      |> TypSlice.temp,
    )
  };

// Slice of a cons is just the slice of the head element + the ids of the cons constructor
let of_list_cons = (ids: list(Id.t), hd_ty: TypSlice.t): t =>
  Just(
    `SliceIncr((Slice(List(hd_ty)), TypSlice.slice_of_ids(ids)))
    |> TypSlice.temp,
  );

// Slice of a concat is just the joined slice of the list arguments + the @ operator ids
let of_list_concat =
    (ids: list(Id.t), ctx: Ctx.t, tys: list(TypSlice.t), c_ids: list(Id.t))
    : t =>
  switch (
    TypSlice.join_all(
      ~empty=`Typ(Unknown(Internal)) |> TypSlice.temp,
      ctx,
      tys,
    )
  ) {
  | None => NoJoin(List, add_source(ids, tys))
  | Some(ty) => Just(TypSlice.(ty |> wrap_incr(slice_of_ids(ids))))
  };

let of_prod = (ids: list(Id.t), tys: list(TypSlice.t)) =>
  Just(
    `SliceIncr((Slice(Prod(tys)), ids |> TypSlice.slice_of_ids))
    |> TypSlice.temp,
  );

// Base types slices should all contain the term's ids
let of_base = (ids: list(Id.t), ty: Typ.term) =>
  Just(
    `SliceGlobal((`Typ(ty), TypSlice.slice_of_ids(ids))) |> TypSlice.temp,
  );
// Operation slices are similarly determined by their ids
let of_op = of_base;

let of_parens = (ids: list(Id.t), ty: TypSlice.t) =>
  Just(ty |> TypSlice.(wrap_incr(slice_of_ids(ids))));
let of_seq = of_parens;
let of_filter = of_seq; // TODO: check
let of_fix = of_seq;
let of_ap_ok = of_seq;

// Holes should be omitted from slices
let hole = Just(`Typ(Unknown(Internal)) |> TypSlice.temp);

// Where arg has been analysed to check if nullary_args
let of_ap =
    (
      ids: list(Id.t),
      ctx: Ctx.t,
      arg_ids: list(Id.t),
      ty_in: TypSlice.t,
      ty_out: TypSlice.t,
    )
    : t =>
  Id.is_nullary_ap_flag(arg_ids)
  && !TypSlice.is_consistent(ctx, ty_in, `Typ(Prod([])) |> TypSlice.temp)
    ? BadTrivAp(ty_in) : ty_out |> of_ap_ok(ids);

let of_typap = (ids: list(Id.t), ctx: Ctx.t, typ_ap: Typ.t, ty: TypSlice.t) => {
  let (option_name, ty_body) = TypSlice.matched_forall(ctx, ty);
  switch (option_name) {
  | Some(name) =>
    Just(
      TypSlice.(
        // TODO: t_of_typ_t_sliced could be done in parsing
        subst(typ_ap |> t_of_typ_t_sliced, name, ty_body)
        |> wrap_incr(slice_of_ids(ids))
      ),
    ) // TODO: Check slices here
  | None => Just(ty_body) /* invalid name matches with no free type variables. */
  };
};

let of_fun =
    (
      ids: list(Id.t),
      is_exhaustive: bool,
      ty_in: TypSlice.t,
      ty_out: TypSlice.t,
    ) => {
  let unwrapped_self: exp =
    Common(
      Just(
        `SliceIncr((
          Slice(Arrow(ty_in, ty_out)),
          TypSlice.slice_of_ids(ids),
        ))
        |> TypSlice.temp,
      ),
    );
  is_exhaustive ? unwrapped_self : InexhaustiveMatch(unwrapped_self);
};

let of_typfun = (ids: list(Id.t), tpat, ty) =>
  Just(
    `SliceIncr((Slice(Forall(tpat, ty)), TypSlice.slice_of_ids(ids)))
    |> TypSlice.temp,
  );

let of_let = (ids: list(Id.t), is_exhaustive: bool, ty: TypSlice.t) => {
  let unwrapped_self: exp =
    Common(Just(TypSlice.(ty |> wrap_incr(slice_of_ids(ids)))));
  is_exhaustive ? unwrapped_self : InexhaustiveMatch(unwrapped_self);
};

// skip_slices will not tag slices to anything if the term is a slice. i.e. not a pure type `Typ(ty)
let of_annot = (~skip_slices=true, ids: list(Id.t), ty: TypSlice.t): t => {
  /*
   // Create type slice corresponding to ids in type. This could be done in parsing instead.
   let rec create_slices = ({term, ids, _} as s: TypSlice.t): TypSlice.t => {
     let (_, rewrap) = IdTagged.unwrap(ty);

     TypSlice.(
       switch (term) {
       // TODO: consider slice with type variables!
       | `Typ(ty) =>
         (
           switch (ty) {
           | (Unknown(_) | Int | Float | Bool | String | Var(_)) as ty =>
             `SliceGlobal((`Typ(ty), slice_of_ids(ids)))
           | List(ty) => (
               `SliceIncr((
                 Slice(List(create_slices(ty |> t_of_typ_t))),
                 slice_of_ids(ids),
               )): term
             )
           | Arrow(ty1, ty2) =>
             `SliceIncr((
               Slice(
                 Arrow(
                   create_slices(ty1 |> t_of_typ_t),
                   create_slices(ty2 |> t_of_typ_t),
                 ),
               ),
               slice_of_ids(ids),
             ))
           | Sum(m) =>
             `SliceIncr((
               Slice(
                 Sum(
                   ConstructorMap.map_vals(
                     ty => create_slices(ty |> t_of_typ_t),
                     m,
                   ),
                 ),
               ),
               slice_of_ids(ids),
             ))
           | Prod(tys) =>
             `SliceIncr((
               Slice(
                 Prod(List.map(ty => create_slices(ty |> t_of_typ_t), tys)),
               ),
               slice_of_ids(ids),
             ))
           | Parens(ty) =>
             `SliceIncr((
               Slice(Parens(create_slices(ty |> t_of_typ_t))),
               slice_of_ids(ids),
             ))
           | Ap(ty1, ty2) =>
             `SliceIncr((
               Slice(
                 Ap(
                   create_slices(ty1 |> t_of_typ_t),
                   create_slices(ty2 |> t_of_typ_t),
                 ),
               ),
               slice_of_ids(ids),
             ))
           | Rec(tpat, ty) =>
             `SliceIncr((
               Slice(Rec(tpat, create_slices(ty |> t_of_typ_t))),
               slice_of_ids(ids),
             ))
           | Forall(tpat, ty) =>
             `SliceIncr((
               Slice(Forall(tpat, create_slices(ty |> t_of_typ_t))),
               slice_of_ids(ids),
             ))
           }
         )
         |> rewrap
       // Skip slices
       | `SliceIncr(_, _)
       | `SliceGlobal(_, _) when skip_slices => s
       // If not skipping slices: continue searching through subterms for `Typs.
       | `SliceIncr(Typ(ty), slice_incr) =>
         create_slices(`Typ(ty) |> rewrap) |> wrap_incr(slice_incr)
       | `SliceIncr(Slice(s), slice_incr) =>
         `SliceIncr((
           Slice(
             switch (s) {
             | List(ty) => List(create_slices(ty))
             | Arrow(ty1, ty2) =>
               Arrow(create_slices(ty1), create_slices(ty2))
             | Sum(m) =>
               Sum(ConstructorMap.map_vals(ty => create_slices(ty), m))
             | Prod(tys) => Prod(List.map(ty => create_slices(ty), tys))
             | Parens(ty) => Parens(create_slices(ty))
             | Ap(ty1, ty2) => Ap(create_slices(ty1), create_slices(ty2))
             | Rec(tpat, ty) => Rec(tpat, create_slices(ty))
             | Forall(tpat, ty) => Forall(tpat, create_slices(ty))
             },
           ),
           slice_incr,
         ))
         |> rewrap
       | `SliceGlobal(s, slice_global) =>
         create_slices((s :> term) |> rewrap) |> wrap_global(slice_global)
       }
     );
   };
   */
  // The annotation should be global
  Just(
    /*create_slices*/ ty |> TypSlice.(wrap_global(slice_of_ids(ids))),
  );
};
