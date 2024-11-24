open Util;
open OptUtil.Syntax;

/* MODE.re

     This module defines the (analytic) type expectation imposed by a term's
      syntactic context, in particular its immediate parent. The most common
      cases are either Syn (no type expectation), or Ana (some type expectation).

      A term's MODE is used in combination with that term's SELF (Self.re) by
      to determine that term's STATUS (Info.re), which dictates whether or not
      it is placed in a hole, and hence its FIXED TYPE (Info.re).

      The provenance of an analytic mode are the ids of the term which put the
      type-checker into analysis mode. i.e. any rule that has conclusion synthesising
      and a premise analysing.
      TODO: It might be required to have analysis analyse against a SLICE, which
            can then be decomposed. Note: Check that every analysis is derived from
            a synthesised type (which has a slice)

      (It is conjectured [citation needed] that the Syn mode is functionally
      indistinguishable from Ana(Unknown(SynSwitch)), and that this type is
      thus vestigial.)

   */

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | SynFun /* Used only in function position of applications */
  | SynTypFun
  | Syn
  | Ana(Slice.t);

let ana = s => Ana(s);

/* The expected type imposed by a mode */
let ty_of: t => Typ.t =
  fun
  | Ana(s) => Slice.ty_of(s)
  | Syn => Unknown(SynSwitch) |> Typ.temp
  | SynFun =>
    Arrow(Unknown(SynSwitch) |> Typ.temp, Unknown(SynSwitch) |> Typ.temp)
    |> Typ.temp
  | SynTypFun =>
    Forall(Var("syntypfun") |> TPat.fresh, Unknown(SynSwitch) |> Typ.temp)
    |> Typ.temp; /* TODO: naming the type variable? */

let of_arrow = (ctx: Ctx.t, mode: t): (t, t) =>
  switch (mode) {
  | Syn
  | SynFun
  | SynTypFun => (Syn, Syn)
  | Ana(s) => s |> Slice.matched_arrow(ctx) |> TupleUtil.map2(ana)
  };

let of_forall = (ctx: Ctx.t, name_opt: option(string), mode: t): t =>
  switch (mode) {
  | Syn
  | SynFun
  | SynTypFun => Syn
  | Ana((ty, _, _)) =>
    let (name_expected_opt, item) = Typ.matched_forall(ctx, ty);
    switch (name_opt, name_expected_opt) {
    | (Some(name), Some(name_expected)) =>
      Ana((
        Typ.subst(Var(name) |> Typ.temp, name_expected, item),
        TEMP,
        Slice.empty,
      )) // TODO
    | _ => Ana((item, TEMP, Slice.empty))
    };
  };

let of_prod = (ctx: Ctx.t, mode: t, length): list(t) =>
  switch (mode) {
  | Syn
  | SynFun
  | SynTypFun => List.init(length, _ => Syn)
  | Ana(s) => s |> Slice.matched_prod(ctx, length) |> List.map(ana)
  };

let of_cons_hd = (ctx: Ctx.t, mode: t, cons_ids: list(Id.t)): t =>
  // cons_ids is the ids of the cons operator for which the hd will be checked by this MODE
  // This cons operator much be included in the slice (TODO: theory for why this is)
  switch (mode) {
  | Syn
  | SynFun
  | SynTypFun => Syn
  | Ana((_, _, c) as s) =>
    Ana(
      Slice.(
        matched_list(ctx, s) |> append(of_ids(cons_ids)) |> append(c)
      ),
    )
  // c is the code slice of why the type is a list. This must be maintained (TODO: theory)
  // Maybe this can be inbuild into Slice.matched_list if this makes sense in general
  };

let of_cons_tl =
    (ctx: Ctx.t, mode: t, hd_s: Slice.t, cons_ids: list(Id.t)): t =>
  switch (mode) {
  | Syn
  | SynFun
  | SynTypFun =>
    Ana((
      List(Slice.ty_of(hd_s)) |> Typ.temp,
      List(hd_s),
      Slice.of_ids(cons_ids),
    ))
  | Ana((_, _, c) as s) =>
    let l_s = Slice.matched_list(ctx, s);
    Ana(
      (
        List(l_s |> Slice.ty_of) |> Typ.temp,
        List(l_s),
        Slice.of_ids(cons_ids),
      )
      |> Slice.append(c),
    );
  };

let of_list = (ctx: Ctx.t, mode: t, lit_ids: list(Id.t)): t =>
  switch (mode) {
  | Syn
  | SynFun
  | SynTypFun => Syn
  | Ana((_, _, c) as s) =>
    Ana(
      Slice.(matched_list(ctx, s) |> append(of_ids(lit_ids)) |> append(c)),
    )
  };

let of_list_concat = (ctx: Ctx.t, mode: t, concat_ids: list(Id.t)): t =>
  switch (mode) {
  | Syn
  | SynFun
  | SynTypFun =>
    Ana(
      Slice.(
        List(Unknown(SynSwitch) |> Typ.temp) |> Typ.temp,
        List((Unknown(SynSwitch) |> Typ.temp, SynSwitch, Slice.empty)),
        Slice.of_ids(concat_ids),
      ),
    )
  | Ana((_, _, _) as s) =>
    let l_s = Slice.matched_list(ctx, s);
    Ana((
      List(l_s |> Slice.ty_of) |> Typ.temp,
      List(l_s),
      Slice.of_ids(concat_ids),
    ));
  };

let of_list_lit =
    (ctx: Ctx.t, length, mode: t, lit_ids: list(Id.t)): list(t) =>
  List.init(length, _ => of_list(ctx, mode, lit_ids));

let ctr_ana_typ = (ctx: Ctx.t, mode: t, ctr: Constructor.t): option(Slice.t) => {
  /* If a ctr is being analyzed against (an arrow type returning)
     a sum type having that ctr as a variant, we consider the
     ctr's type to be determined by the sum type */
  switch (mode) {
  | Ana(({term: Arrow(_, ty_ana), _}, _, _))
  | Ana((ty_ana, _, _)) =>
    let+ ctrs = Typ.get_sum_constructors(ctx, ty_ana);
    let ty_entry = ConstructorMap.get_entry(ctr, ctrs);
    switch (ty_entry) {
    | None => ty_ana |> Slice.(of_ty(empty)) //TODO
    | Some(ty_in) =>
      Arrow(ty_in, ty_ana) |> Typ.temp |> Slice.(of_ty(empty))
    };
  | _ => None
  };
};

let of_ctr_in_ap = (ctx: Ctx.t, mode: t, ctr: Constructor.t): option(t) =>
  switch (ctr_ana_typ(ctx, mode, ctr)) {
  | Some(({term: Arrow(_), _} as ty_ana, _, _)) =>
    Some(Ana(ty_ana |> Slice.(of_ty(empty))))
  | Some((ty_ana, _, _)) =>
    /* Consider for example "let _ : +Yo = Yo("lol") in..."
       Here, the 'Yo' constructor should be in a hole, as it
       is nullary but used as unary; we reflect this by analyzing
       against an arrow type. Since we can't guess at what the
       parameter type might have be, we use Unknown. */
    Some(
      Ana(
        Arrow(Unknown(Internal) |> Typ.temp, ty_ana)
        |> Typ.temp
        |> Slice.(of_ty(empty)),
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

let of_deferred_ap_args = (length: int, ty_ins: list(Typ.t)): list(t) =>
  (
    List.length(ty_ins) == length
      ? ty_ins
      : List.init(length, _ => (Unknown(Internal): Typ.term) |> Typ.temp)
  )
  |> List.map(ty => Ana(ty |> Slice.(of_ty(empty))));
