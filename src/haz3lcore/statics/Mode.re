open Util;
open OptUtil.Syntax;

/* MODE.re

     This module defines the (analytic) type expectation imposed by a term's
      syntactic context, in particular its immediate parent. The most common
      cases are either Syn (no type expectation), or Ana (some type expectation).

      A term's MODE is used in combination with that term's SELF (Self.re) by
      to determine that term's STATUS (Info.re), which dictates whether or not
      it is placed in a hole, and hence its FIXED TYPE (Info.re).

      (It is conjectured [citation needed] that the Syn mode is functionally
      indistinguishable from Ana(Unknown(SynSwitch)), and that this type is
      thus vestigial.)

   */

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | SynFun /* Used only in function position of applications */
  | SynTypFun
  | Syn
  | Ana(TypSlice.t);

let ana: TypSlice.t => t = ty => Ana(ty);

/* The expected type imposed by a mode */
let ty_of: t => TypSlice.t =
  fun
  | Ana(ty) => ty
  | Syn => `Typ(Unknown(SynSwitch)) |> TypSlice.temp
  | SynFun =>
    `Typ(
      Arrow(Unknown(SynSwitch) |> Typ.temp, Unknown(SynSwitch) |> Typ.temp),
    )
    |> TypSlice.temp
  | SynTypFun =>
    `Typ(
      Forall(
        Var("syntypfun") |> TPat.fresh,
        Unknown(SynSwitch) |> Typ.temp,
      ),
    )
    |> TypSlice.temp; /* TODO: naming the type variable? */

//maintain parentheses in slices
let of_parens = (ids, mode: t): t =>
  switch (mode) {
  | Syn
  | SynFun
  | SynTypFun => mode
  | Ana(ty) => Ana(ty |> TypSlice.(wrap_incr(slice_of_ids(ids))))
  };

let of_arrow = (ctx: Ctx.t, mode: t): (t, t) =>
  switch (mode) {
  | Syn
  | SynFun
  | SynTypFun => (Syn, Syn)
  | Ana(ty) => ty |> TypSlice.matched_arrow(ctx) |> TupleUtil.map2(ana)
  };

let of_forall = (ctx: Ctx.t, name_opt: option(string), mode: t): t =>
  switch (mode) {
  | Syn
  | SynFun
  | SynTypFun => Syn
  | Ana(ty) =>
    let (name_expected_opt, item) = TypSlice.matched_forall(ctx, ty);
    switch (name_opt, name_expected_opt) {
    | (Some(name), Some(name_expected)) =>
      Ana(
        TypSlice.subst(
          `Typ(Var(name)) |> TypSlice.temp,
          name_expected,
          item,
        ),
      )
    | _ => Ana(item)
    };
  };

let of_prod = (ids, ctx: Ctx.t, mode: t, length): list(t) =>
  switch (mode) {
  | Syn
  | SynFun
  | SynTypFun => List.init(length, _ => Syn)
  | Ana(ty) =>
    ty
    |> TypSlice.matched_prod(ctx, length)
    |> List.map(TypSlice.(wrap_global(slice_of_ids(ids))))
    |> List.map(ana)
  };

let of_cons_hd = (ids: list(Id.t), ctx: Ctx.t, mode: t): t =>
  switch (mode) {
  | Syn
  | SynFun
  | SynTypFun => Syn
  | Ana(ty) =>
    Ana(TypSlice.(matched_list(ctx, ty) |> wrap_global(slice_of_ids(ids))))
  };

let of_cons_tl = (ids: list(Id.t), ctx: Ctx.t, mode: t, hd_ty: TypSlice.t): t =>
  switch (mode) {
  | Syn
  | SynFun
  | SynTypFun =>
    Ana(
      `SliceIncr((Slice(List(hd_ty)), TypSlice.empty_slice_incr))
      |> TypSlice.temp
      |> TypSlice.(wrap_global(slice_of_ids(ids))),
    )
  | Ana(ty) =>
    Ana(
      `SliceIncr((
        Slice(List(TypSlice.matched_list(ctx, ty))),
        TypSlice.(
          // Re-add incremental slice of list, as we rewrap in a list: TODO - check this
          ty |> term_of |> get_incr_slice_or_empty
        ),
      ))
      |> TypSlice.temp
      |> TypSlice.(wrap_global(slice_of_ids(ids))),
    )
  };

let of_list = (ids: list(Id.t), ctx: Ctx.t, mode: t): t =>
  switch (mode) {
  | Syn
  | SynFun
  | SynTypFun => Syn
  | Ana(ty) =>
    Ana(TypSlice.(matched_list(ctx, ty) |> wrap_global(slice_of_ids(ids))))
  };

let of_list_concat = (ids: list(Id.t), ctx: Ctx.t, mode: t): t =>
  switch (mode) {
  | Syn
  | SynFun
  | SynTypFun =>
    Ana(
      `SliceIncr((
        Typ(List(Unknown(SynSwitch) |> Typ.temp)),
        TypSlice.empty_slice_incr,
      ))
      |> TypSlice.temp
      |> TypSlice.wrap_global(TypSlice.slice_of_ids(ids)),
    )
  | Ana(ty) =>
    Ana(
      `SliceIncr((
        Slice(List(TypSlice.matched_list(ctx, ty))),
        TypSlice.(
          // Re-add incremental slice of list, as we rewrap in a list
          ty |> term_of |> get_incr_slice_or_empty
        ),
      ))
      |> TypSlice.temp
      |> TypSlice.(wrap_global(slice_of_ids(ids))),
    )
  };

let of_list_lit = (ids: list(Id.t), ctx: Ctx.t, length, mode: t): list(t) =>
  List.init(length, _ => of_list(ids, ctx, mode));

let ctr_ana_typ =
    (ctx: Ctx.t, mode: t, ctr: Constructor.t): option(TypSlice.t) => {
  /* If a ctr is being analyzed against (an arrow type returning)
     a sum type having that ctr as a variant, we consider the
     ctr's type to be determined by the sum type */
  switch (mode) {
  | Ana(ty_ana) =>
    let ty_ana =
      switch (TypSlice.matched_arrow_strict(ctx, ty_ana)) {
      | Some((_, ty_ana)) => ty_ana
      | None => ty_ana
      };
    let+ ctrs = TypSlice.get_sum_constructors(ctx, ty_ana);
    let ty_entry = ConstructorMap.get_entry(ctr, ctrs);
    switch (ty_entry) {
    | None => ty_ana
    | Some(ty_in) =>
      `SliceIncr((Slice(Arrow(ty_in, ty_ana)), TypSlice.empty_slice_incr))
      |> TypSlice.temp
    };
  | _ => None
  };
};

let of_ctr_in_ap = (ctx: Ctx.t, mode: t, ctr: Constructor.t): option(t) =>
  switch (ctr_ana_typ(ctx, mode, ctr)) {
  | Some(ty_ana) when TypSlice.is_arrow(ty_ana) => Some(Ana(ty_ana))
  | Some(ty_ana) =>
    /* Consider for example "let _ : +Yo = Yo("lol") in..."
       Here, the 'Yo' constructor should be in a hole, as it
       is nullary but used as unary; we reflect this by analyzing
       against an arrow type. Since we can't guess at what the
       parameter type might have be, we use Unknown. */
    Some(
      Ana(
        `SliceIncr((
          Slice(Arrow(`Typ(Unknown(Internal)) |> TypSlice.temp, ty_ana)),
          TypSlice.empty_slice_incr,
        ))
        |> TypSlice.temp,
      ),
    )
  | None => None
  };

let of_ap = (ctx, mode, ctr: option(Constructor.t)): t =>
  /* If a ctr application is being analyzed against a sum type for
     which that ctr is a variant, then we consider the ctr to be in
     analytic mode against an arrow returning that sum type; otherwise
     we use the typical mode for function applications */
  switch (ctr) {
  | Some(name) =>
    switch (of_ctr_in_ap(ctx, mode, name)) {
    | Some(mode) => mode
    | _ => SynFun
    }
  | None => SynFun
  };

let typap_mode: t = SynTypFun;

let of_deferred_ap_args = (length: int, ty_ins: list(TypSlice.t)): list(t) =>
  (
    List.length(ty_ins) == length
      ? ty_ins
      : List.init(length, _ =>
          (`Typ(Unknown(Internal)): TypSlice.term) |> TypSlice.temp
        )
  )
  |> List.map(ty => Ana(ty));

// Operations are analysed against the return type.
let of_op = (ids: list(Id.t), ty: Typ.term) =>
  Ana(
    `SliceGlobal((`Typ(ty), TypSlice.slice_of_ids(ids))) |> TypSlice.temp,
  );

let of_ann = (ids: list(Id.t), ty: TypSlice.t): t => {
  Ana(ty |> TypSlice.(wrap_global(slice_of_ids(ids))));
};
