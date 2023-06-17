open Sexplib.Std;

module rec Typ: {
  /* TYPE_PROVENANCE: From whence does an unknown type originate?
     Is it generated from an unannotated pattern variable (SynSwitch),
     a pattern variable annotated with a type hole (TypeHole), or
     generated by an internal judgement (Internal)? */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type type_provenance =
    | SynSwitch
    | TypeHole
    | Internal;

  /* TYP.T: Hazel types */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Unknown(type_provenance)
    | Int
    | Float
    | Bool
    | String
    | Var(Token.t)
    | List(t)
    | Arrow(t, t)
    | Sum(sum_map)
    | Prod(list(t))
    | Rec(Token.t, t)
    | Module(Ctx.t)
    | Member(Token.t, t)
  and sum_map = VarMap.t_(option(t));

  [@deriving (show({with_path: false}), sexp, yojson)]
  type sum_entry = (Token.t, option(t));

  /* MODE: The (analytic) type information derived from a term's
     syntactic context. This can either Syn (no type expectation),
     or Ana (a type expectation). It is conjectured [citation needed]
     that the Syn mode is functionally indistinguishable from
     Ana(Unknown(SynSwitch)), and that this type is thus vestigial. */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type mode =
    | SynFun
    | Syn
    | Ana(t);
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type type_provenance =
    | SynSwitch
    | TypeHole
    | Internal;

  /* TYP.T: Hazel types */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Unknown(type_provenance)
    | Int
    | Float
    | Bool
    | String
    | Var(Token.t)
    | List(t)
    | Arrow(t, t)
    | Sum(sum_map)
    | Prod(list(t))
    | Rec(Token.t, t)
    | Module(Ctx.t)
    | Member(Token.t, t)
  and sum_map = VarMap.t_(option(t));

  [@deriving (show({with_path: false}), sexp, yojson)]
  type sum_entry = (Token.t, option(t));

  [@deriving (show({with_path: false}), sexp, yojson)]
  type mode =
    | SynFun
    | Syn
    | Ana(t);
}
and Ctx: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type var_entry = {
    name: Token.t,
    id: Id.t,
    typ: Typ.t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type tvar_entry = {
    name: Token.t,
    id: Id.t,
    kind: Kind.t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type entry =
    | VarEntry(var_entry)
    | TagEntry(var_entry)
    | TVarEntry(tvar_entry);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = list(entry);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type co_entry = {
    id: Id.t,
    mode: Typ.mode,
  };

  /* Each co-context entry is a list of the uses of a variable
     within some scope, including their type demands */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type co = VarMap.t_(list(co_entry));

  let extend: (entry, t) => t;
  let lookup: (t, Token.t) => option(entry);
  let add_abstract: (t, Token.t, Id.t) => t;
  let lookup_tvar: (t, Token.t) => option(tvar_entry);
  let lookup_alias: (t, Token.t) => option(Typ.t);
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type var_entry = {
    name: Token.t,
    id: Id.t,
    typ: Typ.t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type tvar_entry = {
    name: Token.t,
    id: Id.t,
    kind: Kind.t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type entry =
    | VarEntry(var_entry)
    | TagEntry(var_entry)
    | TVarEntry(tvar_entry);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = list(entry);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type co_entry = {
    id: Id.t,
    mode: Typ.mode,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type co = VarMap.t_(list(co_entry));

  let extend = List.cons;

  let lookup = (ctx, name) =>
    List.find_map(
      fun
      | Ctx.VarEntry(v) when v.name == name => Some(VarEntry(v))
      | TagEntry(v) when v.name == name => Some(TagEntry(v))
      | TVarEntry(v) when v.name == name => Some(TVarEntry(v))
      | _ => None,
      ctx,
    );

  let add_abstract = (ctx: t, name: Token.t, id: Id.t): t =>
    extend(TVarEntry({name, id, kind: Abstract}), ctx);

  let lookup_tvar = (ctx: t, name: Token.t): option(tvar_entry) =>
    switch (lookup(ctx, name)) {
    | Some(TVarEntry(t)) => Some(t)
    | _ => None
    };

  let lookup_alias = (ctx: t, t: Token.t): option(Typ.t) =>
    switch (lookup_tvar(ctx, t)) {
    | Some({kind: Singleton(ty), _}) => Some(ty)
    | Some({kind: Abstract, _})
    | _ => None
    };
}
and Kind: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Singleton(Typ.t)
    | Abstract;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Singleton(Typ.t)
    | Abstract;
};
