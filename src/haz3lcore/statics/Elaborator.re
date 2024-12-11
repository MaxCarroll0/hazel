/*
 A nice property would be that elaboration is idempotent...
 */

open Util;

exception MissingTypeInfo;

module Elaboration = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {d: DHExp.t};
};

module ElaborationResult = {
  [@deriving sexp]
  type t =
    | Elaborates(DHExp.t, Slice.t, Delta.t)
    | DoesNotElaborate;
};

let fresh_cast =
    (d: DHExp.t, (t2, _, _) as s1: Slice.t, (t1, _, _) as s2: Slice.t)
    : DHExp.t => {
  Typ.eq(t1, t2)
    ? d
    : {
      let d' =
        Cast(d, s1, Slice.hole) |> DHExp.fresh |> Casts.transition_multiple;
      Cast(d', Slice.hole, s2) |> DHExp.fresh |> Casts.transition_multiple;
    };
};

let fresh_pat_cast = (p: DHPat.t, t1: Typ.t, t2: Typ.t): DHPat.t => {
  Typ.eq(t1, t2)
    ? p
    : {
      Cast(
        DHPat.fresh(Cast(p, t1, Typ.temp(Unknown(Internal))))
        |> Casts.pattern_fixup,
        Typ.temp(Unknown(Internal)),
        t2,
      )
      |> DHPat.fresh
      |> Casts.pattern_fixup;
    };
};

let elaborated_type_slice =
    (m: Statics.Map.t, uexp: UExp.t): (Slice.t, Ctx.t, 'a) => {
  let (mode, self_ty, (_, _, c_s) as self_s, ctx, co_ctx) =
    switch (Id.Map.find_opt(Exp.rep_id(uexp), m)) {
    | Some(Info.InfoExp({mode, ty, ctx, co_ctx, _} as info)) => (
        mode,
        ty,
        Info.exp_slice(info),
        ctx,
        co_ctx,
      )
    | _ => raise(MissingTypeInfo)
    };
  let elab_s: Slice.t =
    switch (mode) {
    | Syn => self_s
    | SynFun =>
      let ((ty1, _, _) as s1, (ty2, _, _) as s2) =
        Slice.matched_arrow(ctx, self_s);
      (Arrow(ty1, ty2) |> Typ.temp, Arrow(s1, s2), c_s);
    | SynTypFun =>
      let (tpat, ty) = Typ.matched_forall(ctx, self_ty);
      let tpat = Option.value(tpat, ~default=TPat.fresh(EmptyHole));
      Forall(tpat, ty) |> Typ.temp |> Slice.temp; // TODO
    // We need to remove the synswitches from this type.
    | Ana(ana_s) => Slice.match_synswitch(ana_s, self_s)
    };
  (elab_s /*|> Slice.normalize(ctx)*/, ctx, co_ctx); // TODO: Normalization
};

let elaborated_pat_slice = (m: Statics.Map.t, upat: UPat.t): (Slice.t, Ctx.t) => {
  let (mode, self_ty, (_, _, c_s) as self_s, ctx, prev_synswitch) =
    switch (Id.Map.find_opt(UPat.rep_id(upat), m)) {
    | Some(Info.InfoPat({mode, ty, ctx, prev_synswitch, _} as info)) => (
        mode,
        ty,
        Info.pat_slice(info),
        ctx,
        prev_synswitch,
      )
    | _ => raise(MissingTypeInfo)
    };
  let elab_s: Slice.t =
    switch (mode) {
    | Syn => self_s
    | SynFun =>
      let ((ty1, _, _) as s1, (ty2, _, _) as s2) =
        Slice.matched_arrow(ctx, self_s);
      (Arrow(ty1, ty2) |> Typ.temp, Arrow(s1, s2), c_s);
    | SynTypFun =>
      let (tpat, ty) = Typ.matched_forall(ctx, self_ty);
      let tpat = Option.value(tpat, ~default=TPat.fresh(EmptyHole));
      Forall(tpat, ty) |> Typ.temp |> Slice.temp; // TODO
    // We need to remove the synswitches from this type.
    | Ana(ana_s) =>
      switch (prev_synswitch) {
      | None => ana_s
      | Some(syn_ty) =>
        Slice.match_synswitch(syn_ty |> Slice.of_ty_with_ids, ana_s) // TODO: prevSynSwitch as slice
      }
    };
  (elab_s /*|> Slice.normalize(ctx)*/, ctx);
};

let rec elaborate_pattern =
        (m: Statics.Map.t, upat: UPat.t): (DHPat.t, Slice.t) => {
  let ((_, _, c) as elaborated_slice, ctx) = elaborated_pat_slice(m, upat);
  let cast_from = (ty, exp) =>
    fresh_pat_cast(exp, ty, elaborated_slice |> Slice.ty_of);
  let (term, rewrap) = UPat.unwrap(upat);
  let dpat =
    switch (term) {
    | Int(_) => upat |> cast_from(Int |> Typ.temp)
    | Bool(_) => upat |> cast_from(Bool |> Typ.temp)
    | Float(_) => upat |> cast_from(Float |> Typ.temp)
    | String(_) => upat |> cast_from(String |> Typ.temp)
    | ListLit(ps) =>
      let (ps, ss) = List.map(elaborate_pattern(m), ps) |> ListUtil.unzip;
      let inner_s: Slice.t = (
        List.map(Slice.ty_of, ss)
        |> Typ.join_all(~empty=Unknown(Internal) |> Typ.temp, ctx)
        |> Option.value(~default=Typ.temp(Unknown(Internal))),
        Join(ctx, ss),
        c,
      );
      ps
      |> List.map2(
           (p, t) => fresh_pat_cast(p, t, inner_s |> Slice.ty_of),
           _,
           List.map(Slice.ty_of, ss),
         )
      |> (
        ps' =>
          ListLit(ps')
          |> rewrap
          |> cast_from(List(inner_s |> Slice.ty_of) |> Typ.temp)
      );
    | Cons(p1, p2) =>
      let (p1', s1) = elaborate_pattern(m, p1);
      let (p2', s2) = elaborate_pattern(m, p2);
      let s2_inner = Slice.matched_list(ctx, s2);
      let s_inner: Slice.t = (
        Typ.join(~fix=false, ctx, s1 |> Slice.ty_of, s2_inner |> Slice.ty_of)
        |> Option.value(~default=Typ.temp(Unknown(Internal))),
        Join(ctx, [s1, s2_inner]),
        c,
      );
      let p1'' =
        fresh_pat_cast(p1', s1 |> Slice.ty_of, s_inner |> Slice.ty_of);
      let p2'' =
        fresh_pat_cast(
          p2',
          s2 |> Slice.ty_of,
          List(s_inner |> Slice.ty_of) |> Typ.temp,
        );
      Cons(p1'', p2'')
      |> rewrap
      |> cast_from(List(s_inner |> Slice.ty_of) |> Typ.temp);
    | Tuple(ps) =>
      let (ps', ss) = List.map(elaborate_pattern(m), ps) |> ListUtil.unzip;
      Tuple(ps')
      |> rewrap
      |> cast_from(Prod(ss |> List.map(Slice.ty_of)) |> Typ.temp);
    | Ap(p1, p2) =>
      let (p1', s1) = elaborate_pattern(m, p1);
      let (p2', s2) = elaborate_pattern(m, p2);
      let (s1l, s1r) = Slice.matched_arrow(ctx, s1);
      let p1'' =
        fresh_pat_cast(
          p1',
          s1 |> Slice.ty_of,
          Arrow(s1l |> Slice.ty_of, s1r |> Slice.ty_of) |> Typ.temp,
        );
      let p2'' = fresh_pat_cast(p2', s2 |> Slice.ty_of, s1l |> Slice.ty_of);
      Ap(p1'', p2'') |> rewrap |> cast_from(s1r |> Slice.ty_of);
    | Invalid(_)
    | EmptyHole
    | MultiHole(_)
    | Wild => upat |> cast_from(Typ.temp(Unknown(Internal)))
    | Var(v) =>
      upat
      |> cast_from(
           Ctx.lookup_var(ctx, v)
           |> Option.map((x: Ctx.var_entry) => x.typ |> Typ.normalize(ctx))
           |> Option.value(~default=Typ.temp(Unknown(Internal))),
         )
    // Type annotations should already appear
    | Parens(p)
    | Cast(p, _, _) =>
      let (p', s) = elaborate_pattern(m, p);
      p' |> cast_from(s |> Slice.ty_of |> Typ.normalize(ctx));
    | Constructor(c, _) =>
      let mode =
        switch (Id.Map.find_opt(Pat.rep_id(upat), m)) {
        | Some(Info.InfoPat({mode, _})) => mode
        | _ => raise(MissingTypeInfo)
        };
      let t =
        switch (Mode.ctr_ana_typ(ctx, mode, c), Ctx.lookup_ctr(ctx, c)) {
        | (Some((ana_ty, _, _)), _) => ana_ty
        | (_, Some({typ: syn_ty, _})) => syn_ty
        | _ => Unknown(Internal) |> Typ.temp
        };
      let t = t |> Typ.normalize(ctx);
      Constructor(c, t) |> rewrap |> cast_from(t);
    };
  (dpat, elaborated_slice);
};

/* The primary goal of elaboration is to convert from a type system
   where we have consistency, to a type system where types are either
   equal or they're not. Anything that was just consistent needs to
   become a cast. [The one other thing elaboration does is make
   recursive let bindings explicit.]

   At the top of this function we work out the "elaborated type" of
   of the expression. We also return this elaborated type so we can
   use it in the recursive call. When elaborate returns, you can trust
   that the returned expression will have the returned type. There is
   however, no guarantee that the returned type is even consistent with
   the "elaborated type" at the top, so you should fresh_cast EVERYWHERE
   just in case.

   Important invariant: any cast in an elaborated expression should have
   normalized types.

   [Matt] A lot of these fresh_cast calls are redundant, however if you
   want to remove one, I'd ask you instead comment it out and leave
   a comment explaining why it's redundant.  */
let rec elaborate = (m: Statics.Map.t, uexp: UExp.t): (DHExp.t, Slice.t) => {
  let ((_, _, c) as elaborated_slice, ctx, co_ctx) =
    elaborated_type_slice(m, uexp);
  let cast_from = (ty, exp) => fresh_cast(exp, ty, elaborated_slice);
  let (term, rewrap) = UExp.unwrap(uexp);
  let dhexp =
    switch (term) {
    | Invalid(_)
    | Undefined
    | EmptyHole => uexp |> cast_from(Slice.hole)
    | MultiHole(stuff) =>
      Any.map_term(
        ~f_exp=(_, exp) => {elaborate(m, exp) |> fst},
        ~f_pat=(_, pat) => {elaborate_pattern(m, pat) |> fst},
        _,
      )
      |> List.map(_, stuff)
      |> (stuff => MultiHole(stuff) |> rewrap |> cast_from(Slice.hole))
    | DynamicErrorHole(e, err) =>
      let (e', _) = elaborate(m, e);
      DynamicErrorHole(e', err) |> rewrap |> cast_from(Slice.hole);
    | Cast(e, _, _) // We remove these casts because they should be re-inserted in the recursive call
    | FailedCast(e, _, _)
    | Parens(e) =>
      let (e', s) = elaborate(m, e);
      e' |> cast_from(s);
    | Deferral(_) => uexp
    | Int(_) => uexp |> cast_from(Int |> Typ.temp |> Slice.of_ty(c))
    | Bool(_) => uexp |> cast_from(Bool |> Typ.temp |> Slice.of_ty(c))
    | Float(_) => uexp |> cast_from(Float |> Typ.temp |> Slice.of_ty(c))
    | String(_) => uexp |> cast_from(String |> Typ.temp |> Slice.of_ty(c))
    | ListLit(es) =>
      let (ds, ss) = List.map(elaborate(m), es) |> ListUtil.unzip;
      let inner_type =
        Typ.join_all(
          ~empty=Unknown(Internal) |> Typ.temp,
          ctx,
          List.map(Slice.ty_of, ss),
        )
        |> Option.value(~default=Typ.temp(Unknown(Internal)));
      let inner_slice: Slice.t = (inner_type, Join(ctx, ss), c);
      let ds' = List.map2((d, t) => fresh_cast(d, t, inner_slice), ds, ss);
      ListLit(ds')
      |> rewrap
      |> cast_from((List(inner_type) |> Typ.temp, List(inner_slice), c));
    | Constructor(c, _) =>
      let mode =
        switch (Id.Map.find_opt(Exp.rep_id(uexp), m)) {
        | Some(Info.InfoExp({mode, _})) => mode
        | _ => raise(MissingTypeInfo)
        };
      let s =
        switch (Mode.ctr_ana_typ(ctx, mode, c), Ctx.lookup_ctr(ctx, c)) {
        | (Some(ana_s), _) => ana_s
        | (_, Some({typ: syn_ty, _})) => syn_ty |> Slice.of_ty_with_ids
        | _ => Slice.hole
        };
      let ty = s |> Slice.ty_of |> Typ.normalize(ctx);
      Constructor(c, ty) |> rewrap |> cast_from(s);
    | Fun(p, e, env, n) =>
      let (p', (typ, _, _) as sp) = elaborate_pattern(m, p);
      let (e', (tye, _, _) as se) = elaborate(m, e);
      Fun(p', e', env, n)
      |> rewrap
      |> cast_from((Arrow(typ, tye) |> Typ.temp, Arrow(sp, se), c));
    | TypFun(tpat, e, name) =>
      let (e', (tye, _, _) as se) = elaborate(m, e);
      TypFun(tpat, e', name)
      |> rewrap
      |> cast_from((Forall(tpat, tye) |> Typ.temp, Forall(tpat, se), c));
    | Tuple(es) =>
      let (ds, ss) = List.map(elaborate(m), es) |> ListUtil.unzip;
      Tuple(ds)
      |> rewrap
      |> cast_from((
           Prod(List.map(Slice.ty_of, ss)) |> Typ.temp,
           Prod(ss),
           c,
         ));
    | Var(v) =>
      uexp
      |> cast_from(
           Ctx.lookup_var(ctx, v)
           |> Option.map((x: Ctx.var_entry) =>
                x.typ
                |> Typ.normalize(ctx)
                |> Slice.(of_ty(union2(of_ctx([VarEntry(x)]), c)))
              )
           |> Option.value(~default=Slice.hole),
         )
    | Let(p, def, body) =>
      let add_name: (option(string), DHExp.t) => DHExp.t = (
        (name, exp) => {
          let (term, rewrap) = DHExp.unwrap(exp);
          switch (term) {
          | Fun(p, e, ctx, _) => Fun(p, e, ctx, name) |> rewrap
          | TypFun(tpat, e, _) => TypFun(tpat, e, name) |> rewrap
          | _ => exp
          };
        }
      );
      let (p, (ty1, _, _) as s1) = elaborate_pattern(m, p);
      let is_recursive =
        Statics.is_recursive(ctx, p, def, ty1)
        && Pat.get_bindings(p)
        |> Option.get
        |> List.exists(f => VarMap.lookup(co_ctx, f) != None);
      if (!is_recursive) {
        let def = add_name(Pat.get_var(p), def);
        let (def, s2) = elaborate(m, def);
        let (body, s) = elaborate(m, body);
        Let(p, fresh_cast(def, s2, s1), body) |> rewrap |> cast_from(s);
      } else {
        // TODO: Add names to mutually recursive functions
        // TODO: Don't add fixpoint if there already is one
        let def = add_name(Option.map(s => s ++ "+", Pat.get_var(p)), def);
        let (def, s2) = elaborate(m, def);
        let (body, s) = elaborate(m, body);
        let fixf = FixF(p, fresh_cast(def, s2, s1), None) |> DHExp.fresh;
        Let(p, fixf, body) |> rewrap |> cast_from(s);
      };
    | FixF(p, e, env) =>
      let (p', sp) = elaborate_pattern(m, p);
      let (e', se) = elaborate(m, e);
      FixF(p', fresh_cast(e', se, sp), env) |> rewrap |> cast_from(sp);
    | TyAlias(_, _, e) =>
      let (e', se) = elaborate(m, e);
      e' |> cast_from(se);
    | Ap(dir, f, a) =>
      let (f', sf) = elaborate(m, f);
      let (a', sa) = elaborate(m, a);
      let (sf1, sf2) = Slice.matched_arrow(ctx, sf);
      let f'' =
        fresh_cast(
          f',
          sf,
          (
            Arrow(Slice.ty_of(sf1), Slice.ty_of(sf2)) |> Typ.temp,
            Arrow(sf1, sf2),
            c,
          ),
        );
      let a'' = fresh_cast(a', sa, sf1);
      Ap(dir, f'', a'') |> rewrap |> cast_from(sf2);
    | DeferredAp(f, args) =>
      let (f', sf) = elaborate(m, f);
      let (args', ss) = List.map(elaborate(m), args) |> ListUtil.unzip;
      let ((_, _, cf1) as sf1, sf2) = Slice.matched_arrow(ctx, sf);
      let s_fargs = Slice.matched_prod(ctx, List.length(args), sf1);
      let f'' =
        fresh_cast(
          f',
          sf,
          (
            Arrow(
              Prod(List.map(Slice.ty_of, s_fargs)) |> Typ.temp,
              Slice.ty_of(sf2),
            )
            |> Typ.temp,
            Arrow(
              (
                Prod(List.map(Slice.ty_of, s_fargs)) |> Typ.temp,
                Prod(s_fargs),
                cf1,
              ),
              sf2,
            ),
            c,
          ),
        );
      let args'' = ListUtil.map3(fresh_cast, args', ss, s_fargs);
      let remaining_args =
        List.filter(
          ((arg, _)) => Exp.is_deferral(arg),
          List.combine(args, s_fargs),
        );
      let remaining_arg_s =
        List.length(remaining_args) == 1
          ? snd(List.hd(remaining_args))
          : (
            Prod(List.map(((_, s)) => Slice.ty_of(s), remaining_args))
            |> Typ.temp,
            Prod(List.map(snd, remaining_args)),
            Slice.empty,
          );
      DeferredAp(f'', args'')
      |> rewrap
      |> cast_from((
           Arrow(remaining_arg_s |> Slice.ty_of, sf2 |> Slice.ty_of)
           |> Typ.temp,
           Arrow(remaining_arg_s, sf2),
           c,
         ));
    | TypAp(e, ut) =>
      let (e', se) = elaborate(m, e);
      let (tpat, se') =
        Typ.matched_forall(ctx, se |> Slice.ty_of)
        |> (((tpat, ty)) => (tpat, Slice.temp(ty)));
      let ut' = Typ.normalize(ctx, ut) |> Slice.temp;
      let se'' =
        Slice.subst(
          ut',
          tpat |> Option.value(~default=TPat.fresh(EmptyHole)),
          se',
        );
      TypAp(e', ut) |> rewrap |> cast_from(se'');
    | If(cond, t, f) =>
      let (c', sc) = elaborate(m, cond);
      let (t', (tyt, _, _) as st) = elaborate(m, t);
      let (f', (tyf, _, _) as sf) = elaborate(m, f);
      let s: Slice.t = (
        Typ.join(~fix=false, ctx, tyt, tyf)
        |> Option.value(~default=Typ.temp(Unknown(Internal))),
        Join(ctx, [st, sf]),
        Slice.empty,
      );
      let c'' = fresh_cast(c', sc, Slice.of_ty(c, Bool |> Typ.temp));
      let t'' = fresh_cast(t', st, s);
      let f'' = fresh_cast(f', sf, s);
      If(c'', t'', f'') |> rewrap |> cast_from(s);
    | Seq(e1, e2) =>
      let (e1', _) = elaborate(m, e1);
      let (e2', s2) = elaborate(m, e2);
      Seq(e1', e2') |> rewrap |> cast_from(s2);
    | Test(e) =>
      let (e', s) = elaborate(m, e);
      Test(fresh_cast(e', s, Bool |> Typ.temp |> Slice.of_ty(c)))
      |> rewrap
      |> cast_from(Prod([]) |> Typ.temp |> Slice.of_ty(c));
    | Filter(kind, e) =>
      let (e', s) = elaborate(m, e);
      let kind' =
        switch (kind) {
        | Residue(_) => kind
        | Filter({act, pat}) => Filter({act, pat: elaborate(m, pat) |> fst})
        };
      Filter(kind', e') |> rewrap |> cast_from(s);
    | Closure(env, e) =>
      // Should we be elaborating the contents of the environment?
      let (e', s) = elaborate(m, e);
      Closure(env, e') |> rewrap |> cast_from(s);
    | Cons(e1, e2) =>
      let (e1', (ty1, _, _) as s1) = elaborate(m, e1);
      let (e2', s2) = elaborate(m, e2);
      let (ty2_inner, _, _) as s2_inner = Slice.matched_list(ctx, s2);
      let (ty_inner, _, _) as s_inner = (
        Typ.join(~fix=false, ctx, ty1, ty2_inner)
        |> Option.value(~default=Typ.temp(Unknown(Internal))),
        Join(ctx, [s1, s2_inner]): Slice.s_ty,
        Slice.empty,
      );
      let e1'' = fresh_cast(e1', s1, s_inner);
      let e2'' =
        fresh_cast(
          e2',
          s2,
          (List(ty_inner) |> Typ.temp, List(s_inner), c),
        );
      Cons(e1'', e2'')
      |> rewrap
      |> cast_from((List(ty_inner) |> Typ.temp, List(s_inner), c));
    | ListConcat(e1, e2) =>
      let (e1', s1) = elaborate(m, e1);
      let (e2', s2) = elaborate(m, e2);
      let s_inner1 = Slice.matched_list(ctx, s1);
      let s_inner2 = Slice.matched_list(ctx, s2);
      let s_inner: Slice.t = (
        Typ.join(
          ~fix=false,
          ctx,
          s_inner1 |> Slice.ty_of,
          s_inner2 |> Slice.ty_of,
        )
        |> Option.value(~default=Typ.temp(Unknown(Internal))),
        Join(ctx, [s_inner1, s_inner2]),
        Slice.empty,
      );
      let e1'' =
        fresh_cast(
          e1',
          s1,
          (List(s_inner |> Slice.ty_of) |> Typ.temp, List(s_inner), c),
        );
      let e2'' =
        fresh_cast(
          e2',
          s2,
          (List(s_inner |> Slice.ty_of) |> Typ.temp, List(s_inner), c),
        );
      ListConcat(e1'', e2'')
      |> rewrap
      |> cast_from((
           List(s_inner |> Slice.ty_of) |> Typ.temp,
           List(s_inner),
           c,
         ));
    | UnOp(Meta(Unquote), e) =>
      switch (e.term) {
      // TODO: confirm whether these types are correct
      | Var("e") =>
        Constructor("$e", Unknown(Internal) |> Typ.temp) |> rewrap
      | Var("v") =>
        Constructor("$v", Unknown(Internal) |> Typ.temp) |> rewrap
      | _ => EmptyHole |> rewrap |> cast_from(Slice.hole)
      }
    | UnOp(Int(Minus), e) =>
      let (e', s) = elaborate(m, e);
      UnOp(
        Int(Minus),
        fresh_cast(e', s, Int |> Typ.temp |> Slice.of_ty(c)),
      )
      |> rewrap
      |> cast_from(Int |> Typ.temp |> Slice.of_ty(c));
    | UnOp(Bool(Not), e) =>
      let (e', s) = elaborate(m, e);
      UnOp(
        Bool(Not),
        fresh_cast(e', s, Bool |> Typ.temp |> Slice.of_ty(c)),
      )
      |> rewrap
      |> cast_from(Bool |> Typ.temp |> Slice.of_ty(c));
    | BinOp(Int(Plus | Minus | Times | Power | Divide) as op, e1, e2) =>
      let (e1', s1) = elaborate(m, e1);
      let (e2', s2) = elaborate(m, e2);
      BinOp(
        op,
        fresh_cast(e1', s1, Int |> Typ.temp |> Slice.of_ty(c)),
        fresh_cast(e2', s2, Int |> Typ.temp |> Slice.of_ty(c)),
      )
      |> rewrap
      |> cast_from(Int |> Typ.temp |> Slice.of_ty(c));
    | BinOp(
        Int(
          LessThan | LessThanOrEqual | GreaterThan | GreaterThanOrEqual |
          Equals |
          NotEquals,
        ) as op,
        e1,
        e2,
      ) =>
      let (e1', s1) = elaborate(m, e1);
      let (e2', s2) = elaborate(m, e2);
      BinOp(
        op,
        fresh_cast(e1', s1, Int |> Typ.temp |> Slice.of_ty(c)),
        fresh_cast(e2', s2, Int |> Typ.temp |> Slice.of_ty(c)),
      )
      |> rewrap
      |> cast_from(Bool |> Typ.temp |> Slice.of_ty(c));
    | BinOp(Bool(And | Or) as op, e1, e2) =>
      let (e1', s1) = elaborate(m, e1);
      let (e2', s2) = elaborate(m, e2);
      BinOp(
        op,
        fresh_cast(e1', s1, Bool |> Typ.temp |> Slice.of_ty(c)),
        fresh_cast(e2', s2, Bool |> Typ.temp |> Slice.of_ty(c)),
      )
      |> rewrap
      |> cast_from(Bool |> Typ.temp |> Slice.of_ty(c));
    | BinOp(Float(Plus | Minus | Times | Divide | Power) as op, e1, e2) =>
      let (e1', s1) = elaborate(m, e1);
      let (e2', s2) = elaborate(m, e2);
      BinOp(
        op,
        fresh_cast(e1', s1, Float |> Typ.temp |> Slice.of_ty(c)),
        fresh_cast(e2', s2, Float |> Typ.temp |> Slice.of_ty(c)),
      )
      |> rewrap
      |> cast_from(Float |> Typ.temp |> Slice.of_ty(c));
    | BinOp(
        Float(
          LessThan | LessThanOrEqual | GreaterThan | GreaterThanOrEqual |
          Equals |
          NotEquals,
        ) as op,
        e1,
        e2,
      ) =>
      let (e1', s1) = elaborate(m, e1);
      let (e2', s2) = elaborate(m, e2);
      BinOp(
        op,
        fresh_cast(e1', s1, Float |> Typ.temp |> Slice.of_ty(c)),
        fresh_cast(e2', s2, Float |> Typ.temp |> Slice.of_ty(c)),
      )
      |> rewrap
      |> cast_from(Bool |> Typ.temp |> Slice.of_ty(c));
    | BinOp(String(Concat) as op, e1, e2) =>
      let (e1', s1) = elaborate(m, e1);
      let (e2', s2) = elaborate(m, e2);
      BinOp(
        op,
        fresh_cast(e1', s1, String |> Typ.temp |> Slice.of_ty(c)),
        fresh_cast(e2', s2, String |> Typ.temp |> Slice.of_ty(c)),
      )
      |> rewrap
      |> cast_from(String |> Typ.temp |> Slice.of_ty(c));
    | BinOp(String(Equals) as op, e1, e2) =>
      let (e1', s1) = elaborate(m, e1);
      let (e2', s2) = elaborate(m, e2);
      BinOp(
        op,
        fresh_cast(e1', s1, String |> Typ.temp |> Slice.of_ty(c)),
        fresh_cast(e2', s2, String |> Typ.temp |> Slice.of_ty(c)),
      )
      |> rewrap
      |> cast_from(Bool |> Typ.temp |> Slice.of_ty(c));
    | BuiltinFun(fn) =>
      uexp
      |> cast_from(
           Ctx.lookup_var(Builtins.ctx_init, fn)
           |> Option.map((x: Ctx.var_entry) =>
                x.typ |> Slice.(of_ty(union2(of_ctx([VarEntry(x)]), c)))
              )
           |> Option.value(~default=Slice.hole),
         )
    | Match(e, cases) =>
      let (e', s) = elaborate(m, e);
      let (ps, es) = ListUtil.unzip(cases);
      let (ps', pss) = List.map(elaborate_pattern(m), ps) |> ListUtil.unzip;
      let joined_ps: Slice.t = (
        Typ.join_all(
          ~empty=Unknown(Internal) |> Typ.temp,
          ctx,
          List.map(Slice.ty_of, pss),
        )
        |> Option.value(~default=Typ.temp(Unknown(Internal))),
        Join(ctx, pss),
        c,
      );
      let ps'' =
        List.map2(
          (p, s) => fresh_pat_cast(p, s, joined_ps |> Slice.ty_of),
          ps',
          List.map(Slice.ty_of, pss),
        );
      let e'' = fresh_cast(e', s, joined_ps);
      let (es', ess) = List.map(elaborate(m), es) |> ListUtil.unzip;
      let joined_es: Slice.t = (
        Typ.join_all(
          ~empty=Unknown(Internal) |> Typ.temp,
          ctx,
          List.map(Slice.ty_of, ess),
        )
        |> Option.value(~default=Typ.temp(Unknown(Internal))),
        Join(ctx, ess),
        c,
      );
      let es'' = List.map2((e, t) => fresh_cast(e, t, joined_es), es', ess);
      Match(e'', List.combine(ps'', es'')) |> rewrap |> cast_from(joined_es);
    };
  (dhexp, elaborated_slice);
};

//let dhexp_of_uexp = Core.Memo.general(~cache_size_bound=1000, dhexp_of_uexp);

/* This function gives a new id to all the types
   in the expression. It does this to get rid of
   all the invalid ids we added to prevent generating
   too many new ids */
let fix_typ_ids =
  Exp.map_term(~f_typ=(cont, e) => e |> IdTagged.new_ids |> cont);

let uexp_elab = (m: Statics.Map.t, uexp: UExp.t): ElaborationResult.t =>
  switch (elaborate(m, uexp)) {
  | exception MissingTypeInfo => DoesNotElaborate
  | (d, ty) => Elaborates(d, ty, Delta.empty)
  };
