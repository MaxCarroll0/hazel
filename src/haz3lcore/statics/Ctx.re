open Util.OptUtil.Syntax;
include TypBase.Ctx;

let lookup_var = (ctx: t, name: string): option(var_entry) =>
  List.find_map(
    fun
    | VarEntry(v) when v.name == name => Some(v)
    | _ => None,
    ctx,
  );

let lookup_tag = (ctx: t, name: string): option(var_entry) =>
  List.find_map(
    fun
    | TagEntry(t) when t.name == name => Some(t)
    | _ => None,
    ctx,
  );

let add_tags = (ctx: t, name: Token.t, id: Id.t, tags: Typ.sum_map): t =>
  List.map(
    ((tag, typ)) =>
      TagEntry({
        name: tag,
        id,
        typ:
          switch (typ) {
          | None => Var(name)
          | Some(typ) => Arrow(typ, Var(name))
          },
      }),
    tags,
  )
  @ ctx;

let subtract_typ = (ctx: t, free: co): co =>
  VarMap.filter(
    ((k, _)) =>
      switch (lookup_var(ctx, k)) {
      | None => true
      | Some(_) => false
      },
    free,
  );

let subtract_prefix = (ctx: t, prefix_ctx: t): option(t) => {
  // NOTE: does not check that the prefix is an actual prefix
  let prefix_length = List.length(prefix_ctx);
  let ctx_length = List.length(ctx);
  if (prefix_length > ctx_length) {
    None;
  } else {
    Some(
      List.rev(
        Util.ListUtil.sublist((prefix_length, ctx_length), List.rev(ctx)),
      ),
    );
  };
};

/* Note: this currently shadows in the case of duplicates */
let union: list(co) => co =
  List.fold_left((free1, free2) => free1 @ free2, []);

module VarSet = Set.Make(Token);

// Note: filter out duplicates when rendering
let filter_duplicates = (ctx: t): t =>
  ctx
  |> List.fold_left(
       ((ctx, term_set, typ_set), entry) => {
         switch (entry) {
         | VarEntry({name, _})
         | TagEntry({name, _}) =>
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

/* Lattice join on types. This is a LUB join in the hazel2
   sense in that any type dominates Unknown. The optional
   resolve parameter specifies whether, in the case of a type
   variable and a succesful join, to return the resolved join type,
   or to return the (first) type variable for readability */
let rec join =
        (~d=[], ~resolve=false, ctx, ty1: Typ.t, ty2: Typ.t): option(Typ.t) => {
  let join' = join(~d, ctx);
  switch (ty1, ty2) {
  | (Unknown(p1), Unknown(p2)) =>
    Some(Unknown(Typ.join_type_provenance(p1, p2)))
  | (Unknown(_), ty)
  | (ty, Unknown(_)) => Some(ty)
  | (Rec(x1, t1), Rec(x2, t2)) => join(~d=[(x1, x2), ...d], ctx, t1, t2)
  | (Rec(_), _) => None
  | (Var(n1), Var(n2)) =>
    if (Typ.var_eq(d, n1, n2)) {
      Some(Var(n1));
    } else {
      let* ty1 = Kind.lookup_alias(ctx, n1);
      let* ty2 = Kind.lookup_alias(ctx, n2);
      let+ ty_join = join'(ty1, ty2);
      resolve ? ty_join : Var(n1);
    }
  | (Var(name), ty)
  | (ty, Var(name)) =>
    let* ty_name = Kind.lookup_alias(ctx, name);
    let+ ty_join = join'(ty_name, ty);
    resolve ? ty_join : Var(name);
  | (Int, Int) => Some(Int)
  | (Int, _) => None
  | (Float, Float) => Some(Float)
  | (Float, _) => None
  | (Bool, Bool) => Some(Bool)
  | (Bool, _) => None
  | (String, String) => Some(String)
  | (String, _) => None
  | (Arrow(ty1_1, ty1_2), Arrow(ty2_1, ty2_2)) =>
    switch (join'(ty1_1, ty2_1), join'(ty1_2, ty2_2)) {
    | (Some(ty1), Some(ty2)) => Some(Arrow(ty1, ty2))
    | _ => None
    }
  | (Arrow(_), _) => None
  | (Prod(tys1), Prod(tys2)) => join_products(~d, ctx, tys1, tys2)
  | (Prod(_), _) => None
  | (Sum(sm1), Sum(sm2)) => join_sums(~d, ctx, sm1, sm2)
  | (Sum(_), _) => None
  | (List(ty_1), List(ty_2)) =>
    switch (join'(ty_1, ty_2)) {
    | Some(ty) => Some(List(ty))
    | None => None
    }
  | (List(_), _) => None
  };
}
and join_products = (~d, ctx: t, tys1, tys2): option(Typ.t) =>
  if (List.length(tys1) != List.length(tys2)) {
    None;
  } else {
    switch (List.map2(join(~d, ctx), tys1, tys2) |> Util.OptUtil.sequence) {
    | None => None
    | Some(tys) => Some(Prod(tys))
    };
  }
and join_sums =
    (~d, ctx: t, sm1: Typ.sum_map, sm2: Typ.sum_map): option(Typ.t) =>
  if (List.length(sm1) != List.length(sm2)) {
    None;
  } else {
    List.map2(
      join_sum_entries(~d, ctx),
      Util.TagMap.sort(sm1),
      Util.TagMap.sort(sm2),
    )
    |> Util.OptUtil.sequence
    |> Option.map(sm => Typ.Sum(sm));
  }
and join_sum_entries =
    (~d, ctx: t, (tag1, ty1): Typ.sum_entry, (tag2, ty2): Typ.sum_entry)
    : option(Typ.sum_entry) =>
  tag1 == tag2
    ? switch (ty1, ty2) {
      | (None, None) => Some((tag1, None))
      | (Some(ty1), Some(ty2)) =>
        let+ ty_join = join(~d, ctx, ty1, ty2);
        (tag1, Some(ty_join));
      | _ => None
      }
    : None;

let join_all = (ctx: t, ts: list(Typ.t)): option(Typ.t) =>
  List.fold_left(
    (acc, ty) => Util.OptUtil.and_then(join(ctx, ty), acc),
    Some(Unknown(Internal)),
    ts,
  );
