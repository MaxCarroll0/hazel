open Sexplib.Std;
open Util;
open OptUtil.Syntax;

let precedence_Prod = 1;
let precedence_Arrow = 2;
let precedence_Sum = 3;
let precedence_Const = 4;

module rec Typ: {
  /* TYPE_PROVENANCE: From whence does an unknown type originate?
     Is it generated from an unannotated pattern variable (SynSwitch),
     a pattern variable annotated with a type hole (TypeHole), or
     generated by an internal judgement (Internal)? */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type type_provenance =
    | SynSwitch
    | TypeHole
    | Free(TypVar.t)
    | Internal;

  /* TYP.T: Hazel types */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Unknown(type_provenance)
    | Int
    | Float
    | Bool
    | String
    | Var(TypVar.t)
    | List(t)
    | Arrow(t, t)
    | Sum(sum_map)
    | Prod(list(t))
    | Rec(TypVar.t, t)
  and sum_map = ConstructorMap.t(option(t));

  [@deriving (show({with_path: false}), sexp, yojson)]
  type sum_entry = ConstructorMap.binding(option(t));

  /* Hazel type annotated with a relevant source location.
     Currently used to track match branches for inconsistent
     branches errors, but could perhaps be used more broadly
     for type debugging UI. */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type source = {
    id: Id.t,
    ty: t,
  };

  let to_string: (~holes: type_provenance => string=?, t) => string;
  let of_source: list(source) => list(t);
  let join_type_provenance:
    (type_provenance, type_provenance) => type_provenance;
  let matched_arrow: (Ctx.t, t) => (t, t);
  let matched_prod: (Ctx.t, int, t) => list(t);
  let matched_list: (Ctx.t, t) => t;
  let precedence: t => int;
  let subst: (t, TypVar.t, t) => t;
  let unroll: t => t;
  let eq: (t, t) => bool;
  let free_vars: (~bound: list(Var.t)=?, t) => list(Var.t);
  let join: (~resolve: bool=?, ~fix: bool, Ctx.t, t, t) => option(t);
  let join_fix: (~resolve: bool=?, Ctx.t, t, t) => option(t);
  let join_all: (~empty: t, Ctx.t, list(t)) => option(t);
  let is_consistent: (Ctx.t, t, t) => bool;
  let weak_head_normalize: (Ctx.t, t) => t;
  let normalize: (Ctx.t, t) => t;
  let sum_entry: (Constructor.t, sum_map) => option(sum_entry);
  let get_sum_constructors: (Ctx.t, t) => option(sum_map);
  let is_unknown: t => bool;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type type_provenance =
    | SynSwitch
    | TypeHole
    | Free(TypVar.t)
    | Internal;

  /* TYP.T: Hazel types */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Unknown(type_provenance)
    | Int
    | Float
    | Bool
    | String
    | Var(TypVar.t)
    | List(t)
    | Arrow(t, t)
    | Sum(sum_map)
    | Prod(list(t))
    | Rec(TypVar.t, t)
  and sum_map = ConstructorMap.t(option(t));

  [@deriving (show({with_path: false}), sexp, yojson)]
  type sum_entry = ConstructorMap.binding(option(t));

  [@deriving (show({with_path: false}), sexp, yojson)]
  type source = {
    id: Id.t,
    ty: t,
  };

  let rec to_string = (~holes=_ => "?", t: t): string => {
    let s = to_string(~holes);
    switch (t) {
    | Int => "Int"
    | Float => "Float"
    | Bool => "Bool"
    | String => "String"
    | Unknown(prov) => holes(prov)
    | Arrow(t1, t2) => "(" ++ s(t1) ++ " -> " ++ s(t2) ++ ")"
    | Prod(tys) => "(" ++ String.concat(", ", List.map(s, tys)) ++ ")"
    | Sum(sm) =>
      let entry = ((ctr, ty)) =>
        switch (ty) {
        | None => ctr
        | Some(t) => ctr ++ "(" ++ s(t) ++ ")"
        };
      "(" ++ String.concat(" + ", List.map(entry, sm)) ++ ")";
    | Rec(x, ty) => "rec " ++ x ++ ".{" ++ s(ty) ++ "}"
    | List(ty) => "[" ++ s(ty) ++ "]"
    | Var(x) => x
    };
  };

  /* Strip location information from a list of sources */
  let of_source = List.map((source: source) => source.ty);

  /* How type provenance information should be collated when
     joining unknown types. This probably requires more thought,
     but right now TypeHole strictly predominates over Internal
     which strictly predominates over SynSwitch. */
  let join_type_provenance =
      (p1: type_provenance, p2: type_provenance): type_provenance =>
    switch (p1, p2) {
    | (Free(tv1), Free(tv2)) when TypVar.eq(tv1, tv2) => Free(tv1)
    | (TypeHole, TypeHole | SynSwitch)
    | (SynSwitch, TypeHole) => TypeHole
    | (SynSwitch, Internal)
    | (Internal, SynSwitch) => SynSwitch
    | (Internal | Free(_), _)
    | (_, Internal | Free(_)) => Internal
    | (SynSwitch, SynSwitch) => SynSwitch
    };

  let precedence = (ty: t): int =>
    switch (ty) {
    | Int
    | Float
    | Bool
    | String
    | Unknown(_)
    | Var(_)
    | Rec(_)
    | Sum(_) => precedence_Sum
    | List(_) => precedence_Const
    | Prod(_) => precedence_Prod
    | Arrow(_, _) => precedence_Arrow
    };

  let rec subst = (s: t, x: TypVar.t, ty: t) => {
    switch (ty) {
    | Int => Int
    | Float => Float
    | Bool => Bool
    | String => String
    | Unknown(prov) => Unknown(prov)
    | Arrow(ty1, ty2) => Arrow(subst(s, x, ty1), subst(s, x, ty2))
    | Prod(tys) => Prod(List.map(subst(s, x), tys))
    | Sum(sm) => Sum(ConstructorMap.map(Option.map(subst(s, x)), sm))
    | Rec(y, ty) when TypVar.eq(x, y) => Rec(y, ty)
    | Rec(y, ty) => Rec(y, subst(s, x, ty))
    | List(ty) => List(subst(s, x, ty))
    | Var(y) => TypVar.eq(x, y) ? s : Var(y)
    };
  };

  let unroll = (ty: t): t =>
    switch (ty) {
    | Rec(x, ty_body) => subst(ty, x, ty_body)
    | _ => ty
    };

  /* Type Equality: At the moment, this coincides with alpha equivalence,
     but this will change when polymorphic types are implemented */
  let rec eq = (t1: t, t2: t): bool => {
    switch (t1, t2) {
    | (Rec(x1, t1), Rec(x2, t2)) => eq(t1, subst(Var(x1), x2, t2))
    | (Rec(_), _) => false
    | (Int, Int) => true
    | (Int, _) => false
    | (Float, Float) => true
    | (Float, _) => false
    | (Bool, Bool) => true
    | (Bool, _) => false
    | (String, String) => true
    | (String, _) => false
    | (Unknown(_), Unknown(_)) => true
    | (Unknown(_), _) => false
    | (Arrow(t1, t2), Arrow(t1', t2')) => eq(t1, t1') && eq(t2, t2')
    | (Arrow(_), _) => false
    | (Prod(tys1), Prod(tys2)) => List.equal(eq, tys1, tys2)
    | (Prod(_), _) => false
    | (List(t1), List(t2)) => eq(t1, t2)
    | (List(_), _) => false
    | (Sum(sm1), Sum(sm2)) =>
      ConstructorMap.equal(Option.equal(eq), sm1, sm2)
    | (Sum(_), _) => false
    | (Var(n1), Var(n2)) => n1 == n2
    | (Var(_), _) => false
    };
  };

  let rec free_vars = (~bound=[], ty: t): list(Var.t) =>
    switch (ty) {
    | Unknown(_)
    | Int
    | Float
    | Bool
    | String => []
    | Var(v) => List.mem(v, bound) ? [] : [v]
    | List(ty) => free_vars(~bound, ty)
    | Arrow(t1, t2) => free_vars(~bound, t1) @ free_vars(~bound, t2)
    | Sum(sm) =>
      ListUtil.flat_map(
        fun
        | None => []
        | Some(typ) => free_vars(~bound, typ),
        List.map(snd, sm),
      )
    | Prod(tys) => ListUtil.flat_map(free_vars(~bound), tys)
    | Rec(x, ty) => free_vars(~bound=[x, ...bound], ty)
    };

  /* Lattice join on types. This is a LUB join in the hazel2
     sense in that any type dominates Unknown. The optional
     resolve parameter specifies whether, in the case of a type
     variable and a succesful join, to return the resolved join type,
     or to return the (first) type variable for readability */
  let rec join =
          (~resolve=false, ~fix, ctx: Ctx.t, ty1: t, ty2: t): option(t) => {
    let join' = join(~resolve, ~fix, ctx);
    switch (ty1, ty2) {
    | (_, Unknown(TypeHole | Free(_)) as ty) when fix =>
      /* NOTE(andrew): This is load bearing
         for ensuring that function literals get appropriate
         casts. Examples/Dynamics has regression tests */
      Some(ty)
    | (Unknown(p1), Unknown(p2)) =>
      Some(Unknown(join_type_provenance(p1, p2)))
    | (Unknown(_), ty)
    | (ty, Unknown(Internal | SynSwitch)) => Some(ty)
    | (Var(n1), Var(n2)) =>
      if (n1 == n2) {
        Some(Var(n1));
      } else {
        let* ty1 = Ctx.lookup_alias(ctx, n1);
        let* ty2 = Ctx.lookup_alias(ctx, n2);
        let+ ty_join = join'(ty1, ty2);
        !resolve && eq(ty1, ty_join) ? Var(n1) : ty_join;
      }
    | (Var(name), ty)
    | (ty, Var(name)) =>
      let* ty_name = Ctx.lookup_alias(ctx, name);
      let+ ty_join = join'(ty_name, ty);
      !resolve && eq(ty_name, ty_join) ? Var(name) : ty_join;
    /* Note: Ordering of Unknown, Var, and Rec above is load-bearing! */
    | (Rec(x1, ty1), Rec(x2, ty2)) =>
      /* TODO:
           This code isn't fully correct, as we may be doing
           substitution on open terms; if x1 occurs in ty2,
           we should be substituting x1 for a fresh variable
           in ty2. This is annoying, and should be obviated
           by the forthcoming debruijn index implementation
         */
      let ctx = Ctx.extend_dummy_tvar(ctx, x1);
      let+ ty_body =
        join(~resolve, ~fix, ctx, ty1, subst(Var(x1), x2, ty2));
      Rec(x1, ty_body);
    | (Rec(_), _) => None
    | (Int, Int) => Some(Int)
    | (Int, _) => None
    | (Float, Float) => Some(Float)
    | (Float, _) => None
    | (Bool, Bool) => Some(Bool)
    | (Bool, _) => None
    | (String, String) => Some(String)
    | (String, _) => None
    | (Arrow(ty1, ty2), Arrow(ty1', ty2')) =>
      let* ty1 = join'(ty1, ty1');
      let+ ty2 = join'(ty2, ty2');
      Arrow(ty1, ty2);
    | (Arrow(_), _) => None
    | (Prod(tys1), Prod(tys2)) =>
      let* tys = ListUtil.map2_opt(join', tys1, tys2);
      let+ tys = OptUtil.sequence(tys);
      Prod(tys);
    | (Prod(_), _) => None
    | (Sum(sm1), Sum(sm2)) =>
      let (sorted1, sorted2) =
        /* If same order, retain order for UI */
        ConstructorMap.same_constructors_same_order(sm1, sm2)
          ? (sm1, sm2)
          : (ConstructorMap.sort(sm1), ConstructorMap.sort(sm2));
      let* ty =
        ListUtil.map2_opt(
          join_sum_entries(~resolve, ~fix, ctx),
          sorted1,
          sorted2,
        );
      let+ ty = OptUtil.sequence(ty);
      Sum(ty);
    | (Sum(_), _) => None
    | (List(ty1), List(ty2)) =>
      let+ ty = join'(ty1, ty2);
      List(ty);
    | (List(_), _) => None
    };
  }
  and join_sum_entries =
      (
        ~resolve,
        ~fix,
        ctx: Ctx.t,
        (ctr1, ty1): sum_entry,
        (ctr2, ty2): sum_entry,
      )
      : option(sum_entry) =>
    switch (ty1, ty2) {
    | (None, None) when ctr1 == ctr2 => Some((ctr1, None))
    | (Some(ty1), Some(ty2)) when ctr1 == ctr2 =>
      let+ ty_join = join(~resolve, ~fix, ctx, ty1, ty2);
      (ctr1, Some(ty_join));
    | _ => None
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

  let rec weak_head_normalize = (ctx: Ctx.t, ty: t): t =>
    switch (ty) {
    | Var(x) =>
      switch (Ctx.lookup_alias(ctx, x)) {
      | Some(ty) => weak_head_normalize(ctx, ty)
      | None => ty
      }
    | _ => ty
    };

  let rec normalize = (ctx: Ctx.t, ty: t): t => {
    switch (ty) {
    | Var(x) =>
      switch (Ctx.lookup_alias(ctx, x)) {
      | Some(ty) => normalize(ctx, ty)
      | None => ty
      }
    | Unknown(_)
    | Int
    | Float
    | Bool
    | String => ty
    | List(t) => List(normalize(ctx, t))
    | Arrow(t1, t2) => Arrow(normalize(ctx, t1), normalize(ctx, t2))
    | Prod(ts) => Prod(List.map(normalize(ctx), ts))
    | Sum(ts) => Sum(ConstructorMap.map(Option.map(normalize(ctx)), ts))
    | Rec(name, ty) =>
      /* NOTE: Dummy tvar added has fake id but shouldn't matter
         as in current implementation Recs do not occur in the
         surface syntax, so we won't try to jump to them. */
      Rec(name, normalize(Ctx.extend_dummy_tvar(ctx, name), ty))
    };
  };

  let matched_arrow = (ctx, ty) =>
    switch (weak_head_normalize(ctx, ty)) {
    | Arrow(ty_in, ty_out) => (ty_in, ty_out)
    | Unknown(SynSwitch) => (Unknown(SynSwitch), Unknown(SynSwitch))
    | _ => (Unknown(Internal), Unknown(Internal))
    };

  let matched_prod = (ctx, length, ty) =>
    switch (weak_head_normalize(ctx, ty)) {
    | Prod(tys) when List.length(tys) == length => tys
    | Unknown(SynSwitch) => List.init(length, _ => Unknown(SynSwitch))
    | _ => List.init(length, _ => Unknown(Internal))
    };

  let matched_list = (ctx, ty) =>
    switch (weak_head_normalize(ctx, ty)) {
    | List(ty) => ty
    | Unknown(SynSwitch) => Unknown(SynSwitch)
    | _ => Unknown(Internal)
    };

  let sum_entry = (ctr: Constructor.t, ctrs: sum_map): option(sum_entry) =>
    List.find_map(
      fun
      | (t, typ) when Constructor.equal(t, ctr) => Some((t, typ))
      | _ => None,
      ctrs,
    );

  let get_sum_constructors = (ctx: Ctx.t, ty: t): option(sum_map) => {
    let ty = weak_head_normalize(ctx, ty);
    switch (ty) {
    | Sum(sm) => Some(sm)
    | Rec(_) =>
      /* Note: We must unroll here to get right ctr types;
         otherwise the rec parameter will leak */
      switch (unroll(ty)) {
      | Sum(sm) => Some(sm)
      | _ => None
      }
    | _ => None
    };
  };

  let is_unknown = (ty: t): bool =>
    switch (ty) {
    | Unknown(_) => true
    | _ => false
    };
}
and Ctx: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type var_entry = {
    name: Var.t,
    id: Id.t,
    typ: Typ.t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type tvar_entry = {
    name: TypVar.t,
    id: Id.t,
    kind: Kind.t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type entry =
    | VarEntry(var_entry)
    | ConstructorEntry(var_entry)
    | TVarEntry(tvar_entry);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = list(entry);

  let extend: (t, entry) => t;
  let extend_tvar: (t, tvar_entry) => t;
  let extend_alias: (t, TypVar.t, Id.t, Typ.t) => t;
  let extend_dummy_tvar: (t, TypVar.t) => t;
  let lookup_tvar: (t, TypVar.t) => option(tvar_entry);
  let lookup_alias: (t, TypVar.t) => option(Typ.t);
  let get_id: entry => Id.t;
  let lookup_var: (t, string) => option(var_entry);
  let lookup_ctr: (t, string) => option(var_entry);
  let is_alias: (t, TypVar.t) => bool;
  let add_ctrs: (t, TypVar.t, Id.t, Typ.sum_map) => t;
  let subtract_prefix: (t, t) => option(t);
  let added_bindings: (t, t) => t;
  let filter_duplicates: t => t;
  let shadows_typ: (t, TypVar.t) => bool;
  let to_string: t => string;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type var_entry = {
    name: Var.t,
    id: Id.t,
    typ: Typ.t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type tvar_entry = {
    name: TypVar.t,
    id: Id.t,
    kind: Kind.t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type entry =
    | VarEntry(var_entry)
    | ConstructorEntry(var_entry)
    | TVarEntry(tvar_entry);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = list(entry);

  let extend = (ctx, entry) => List.cons(entry, ctx);

  let extend_tvar = (ctx: t, tvar_entry: tvar_entry): t =>
    extend(ctx, TVarEntry(tvar_entry));

  let extend_alias = (ctx: t, name: TypVar.t, id: Id.t, ty: Typ.t): t =>
    extend_tvar(ctx, {name, id, kind: Singleton(ty)});

  let extend_dummy_tvar = (ctx: t, name: TypVar.t) =>
    extend_tvar(ctx, {kind: Abstract, name, id: Id.invalid});

  let lookup_tvar = (ctx: t, name: TypVar.t): option(tvar_entry) =>
    List.find_map(
      fun
      | TVarEntry(v) when v.name == name => Some(v)
      | _ => None,
      ctx,
    );

  let lookup_alias = (ctx: t, t: TypVar.t): option(Typ.t) =>
    switch (lookup_tvar(ctx, t)) {
    | Some({kind: Singleton(ty), _}) => Some(ty)
    | Some({kind: Abstract, _})
    | None => None
    };

  let get_id: entry => Id.t =
    fun
    | VarEntry({id, _})
    | ConstructorEntry({id, _})
    | TVarEntry({id, _}) => id;

  let lookup_var = (ctx: t, name: string): option(var_entry) =>
    List.find_map(
      fun
      | VarEntry(v) when v.name == name => Some(v)
      | _ => None,
      ctx,
    );

  let lookup_ctr = (ctx: t, name: string): option(var_entry) =>
    List.find_map(
      fun
      | ConstructorEntry(t) when t.name == name => Some(t)
      | _ => None,
      ctx,
    );

  let is_alias = (ctx: t, name: TypVar.t): bool =>
    switch (lookup_alias(ctx, name)) {
    | Some(_) => true
    | None => false
    };

  let add_ctrs = (ctx: t, name: TypVar.t, id: Id.t, ctrs: Typ.sum_map): t =>
    List.map(
      ((ctr, typ)) =>
        ConstructorEntry({
          name: ctr,
          id,
          typ:
            switch (typ) {
            | None => Var(name)
            | Some(typ) => Arrow(typ, Var(name))
            },
        }),
      ctrs,
    )
    @ ctx;

  let subtract_prefix = (ctx: t, prefix_ctx: t): option(t) => {
    // NOTE: does not check that the prefix is an actual prefix
    let prefix_length = List.length(prefix_ctx);
    let ctx_length = List.length(ctx);
    if (prefix_length > ctx_length) {
      None;
    } else {
      Some(
        List.rev(
          ListUtil.sublist((prefix_length, ctx_length), List.rev(ctx)),
        ),
      );
    };
  };

  let added_bindings = (ctx_after: t, ctx_before: t): t => {
    /* Precondition: new_ctx is old_ctx plus some new bindings */
    let new_count = List.length(ctx_after) - List.length(ctx_before);
    switch (ListUtil.split_n_opt(new_count, ctx_after)) {
    | Some((ctx, _)) => ctx
    | _ => []
    };
  };

  module VarSet = Set.Make(Var);

  // Note: filter out duplicates when rendering
  let filter_duplicates = (ctx: t): t =>
    ctx
    |> List.fold_left(
         ((ctx, term_set, typ_set), entry) => {
           switch (entry) {
           | VarEntry({name, _})
           | ConstructorEntry({name, _}) =>
             VarSet.mem(name, term_set)
               ? (ctx, term_set, typ_set)
               : ([entry, ...ctx], VarSet.add(name, term_set), typ_set)
           | TVarEntry({name, _}) =>
             VarSet.mem(name, typ_set)
               ? (ctx, term_set, typ_set)
               : ([entry, ...ctx], term_set, VarSet.add(name, typ_set))
           }
         },
         ([], VarSet.empty, VarSet.empty),
       )
    |> (((ctx, _, _)) => List.rev(ctx));

  let shadows_typ = (ctx: t, name: TypVar.t): bool =>
    Form.is_base_typ(name) || lookup_alias(ctx, name) != None;

  let to_string = (ctx: t): string =>
    ctx
    |> List.map(
         fun
         | VarEntry({name, typ, _}) => name ++ ": " ++ Typ.to_string(typ)
         | ConstructorEntry({name, typ, _}) =>
           name ++ ": " ++ Typ.to_string(typ)
         | TVarEntry({name, kind, _}) =>
           name ++ ":: " ++ Kind.to_string(kind),
       )
    |> String.concat(", ")
    |> (x => "{" ++ x ++ "}");
}
and Kind: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Singleton(Typ.t)
    | Abstract;
  let to_string: t => string;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Singleton(Typ.t)
    | Abstract;
  let to_string = (kind: t): string =>
    switch (kind) {
    | Singleton(ty) => Typ.to_string(ty)
    | Abstract => "Abstract"
    };
};
