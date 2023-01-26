open Sexplib.Std;

/* TYPE_PROVENANCE: From whence does an unknown type originate?
   Is it generated from an unannotated pattern variable (SynSwitch),
   a pattern variable annotated with a type hole (TypeHole), or
   generated by an internal judgement (Internal)? */
[@deriving (show({with_path: false}), sexp, yojson)]
type type_provenance =
  | SynSwitch
  | TypeHole
  | Internal;

[@deriving (show({with_path: false}), sexp, yojson)]
type ann('item) = {
  item: 'item,
  ann: string,
};

/* TYP.T: Hazel types */
[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Unknown(type_provenance)
  | Int
  | Float
  | Bool
  | String
  | Var(ann(option(int)))
  | List(t)
  | Arrow(t, t)
  | LabelSum(list(tagged))
  | Sum(t, t) // unused
  | Prod(list(t))
  | Rec(ann(t))
  | Forall(ann(t))
and tagged = {
  tag: Token.t,
  typ: option(t),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type adt = (Token.t, list(tagged));

let sort_tagged: list(tagged) => list(tagged) =
  List.sort(({tag: t1, _}, {tag: t2, _}) => compare(t1, t2));

let find_tag = (t: Token.t, tags: list(tagged)): option(tagged) =>
  List.find_map(
    fun
    | {tag, typ} when tag == t => Some({tag, typ})
    | _ => None,
    tags,
  );

let ana_sum = (tag: Token.t, tags: list(tagged), ty_ana: t): option(t) =>
  /* Returns the type of a tag if that tag is given a type by the sum
     type ty_ana having tags as variants. If tag is a nullart constructor,
     ty_ana itself is returned; otherwise an arrow from tag's parameter
     type to ty_ana */
  switch (find_tag(tag, tags)) {
  | Some({typ: Some(ty_in), _}) => Some(Arrow(ty_in, ty_ana))
  | Some({typ: None, _}) => Some(ty_ana)
  | None => None
  };

/* SOURCE: Hazel type annotated with a relevant source location.
   Currently used to track match branches for inconsistent
   branches errors, but could perhaps be used more broadly
   for type debugging UI. */
[@deriving (show({with_path: false}), sexp, yojson)]
type source = {
  id: int,
  ty: t,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type free_errors =
  | Variable
  | Tag
  | TypeVariable;

[@deriving (show({with_path: false}), sexp, yojson)]
type self_error =
  | NoFun(t)
  //| TagArity
  //| MissingTag
  | Free(free_errors);

/* SELF: The (synthetic) type information derivable from a term
   in isolation, using the typing context but not the syntactic
   context. This can either be Free (no type, in the case of
   unbound/undefined names), Joined (a list of types, possibly
   inconsistent, generated by branching forms like ifs,
   matches, and list literals), or Just a regular type. */
[@deriving (show({with_path: false}), sexp, yojson)]
type self =
  | Just(t)
  // TODO: make it so that joined applies only to inconsistent types; rename NoJoin
  | Joined(t => t, list(source))
  | Multi
  | Self(self_error);

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

/* Strip location information from a list of sources */
let source_tys = List.map((source: source) => source.ty);

/* How type provenance information should be collated when
   joining unknown types. This probably requires more thought,
   but right now TypeHole strictly predominates over Internal
   which strictly predominates over SynSwitch. */
let join_type_provenance =
    (p1: type_provenance, p2: type_provenance): type_provenance =>
  switch (p1, p2) {
  | (TypeHole, TypeHole | Internal | SynSwitch)
  | (Internal | SynSwitch, TypeHole) => TypeHole
  | (Internal, Internal | SynSwitch)
  | (SynSwitch, Internal) => Internal
  | (SynSwitch, SynSwitch) => SynSwitch
  };

/* MATCHED JUDGEMENTS: Note that matched judgements work
   a bit different than hazel2 here since hole fixing is
   implicit. Somebody should check that what I'm doing
   here actually makes sense -Andrew */

let matched_arrow: t => (t, t) =
  fun
  | Arrow(ty_in, ty_out) => (ty_in, ty_out)
  | Unknown(prov) => (Unknown(prov), Unknown(prov))
  | _ => (Unknown(Internal), Unknown(Internal));

let matched_forall: t => ann(t) =
  fun
  | Forall(ann) => ann
  | Unknown(prov) => {item: Unknown(prov), ann: "expected_forall"}
  | _ => {item: Unknown(Internal), ann: "expected_forall"};

let matched_rec: t => ann(t) =
  fun
  | Rec(ann) => ann
  | Unknown(prov) => {item: Unknown(prov), ann: "expected_rec"}
  | _ => {item: Unknown(Internal), ann: "expected_rec"};

let matched_arrow_mode: mode => (mode, mode) =
  fun
  | SynFun
  | Syn => (Syn, Syn)
  | Ana(ty) => {
      let (ty_in, ty_out) = matched_arrow(ty);
      (Ana(ty_in), Ana(ty_out));
    };

let matched_forall_mode: mode => mode =
  fun
  | SynFun
  | Syn => Syn
  | Ana(ty) => {
      let ann = matched_forall(ty);
      Ana(ann.item);
    };

let matched_rec_mode: mode => mode =
  fun
  | SynFun
  | Syn => Syn
  | Ana(ty) => {
      let ann = matched_rec(ty);
      Ana(ann.item);
    };

let matched_prod_mode = (mode: mode, length): list(mode) =>
  switch (mode) {
  | Ana(Prod(ana_tys)) when List.length(ana_tys) == length =>
    List.map(ty => Ana(ty), ana_tys)
  | Ana(Unknown(prod)) => List.init(length, _ => Ana(Unknown(prod)))
  | _ => List.init(length, _ => Syn)
  };

let matched_list: t => t =
  fun
  | List(ty) => ty
  | Unknown(prov) => Unknown(prov)
  | _ => Unknown(Internal);

let matched_list_mode: mode => mode =
  fun
  | SynFun
  | Syn => Syn
  | Ana(ty) => Ana(matched_list(ty));

let ap_mode: mode = SynFun;

/* Legacy code from HTyp */

let precedence_Prod = 1;
let precedence_Arrow = 2;
let precedence_Sum = 3;
let precedence_const = 4;
let precedence = (ty: t): int =>
  switch (ty) {
  | Int
  | Float
  | Bool
  | String
  | Unknown(_)
  | Var(_)
  | Rec(_)
  | LabelSum(_)
  | List(_) => precedence_const
  | Prod(_)
  | Forall(_) => precedence_Prod
  | Sum(_, _) => precedence_Sum
  | Arrow(_, _) => precedence_Arrow
  };

/* equality
   At the moment, this coincides with default equality,
   but this will change when polymorphic types are implemented */
let rec eq = (t1, t2) => {
  switch (t1, t2) {
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
  | (Arrow(t1_1, t1_2), Arrow(t2_1, t2_2)) =>
    eq(t1_1, t2_1) && eq(t1_2, t2_2)
  | (Arrow(_), _) => false
  | (Prod(tys1), Prod(tys2)) =>
    List.length(tys1) == List.length(tys2) && List.for_all2(eq, tys1, tys2)
  | (Prod(_), _) => false
  | (Sum(t1_1, t1_2), Sum(t2_1, t2_2)) => eq(t1_1, t2_1) && eq(t1_2, t2_2)
  | (Sum(_), _) => false
  | (List(t1), List(t2)) => eq(t1, t2)
  | (List(_), _) => false
  | (LabelSum(tys1), LabelSum(tys2)) =>
    let (tys1, tys2) = (sort_tagged(tys1), sort_tagged(tys2));
    List.length(tys1) == List.length(tys2)
    && List.for_all2(tagged_eq, tys1, tys2);
  | (LabelSum(_), _) => false
  | (Var({item: x1, _}), Var({item: x2, _})) => x1 == x2
  | (Var(_), _) => false
  | (Rec({item: t1, _}), Rec({item: t2, _})) => eq(t1, t2)
  | (Rec(_), _) => false
  | (Forall({item: t1, _}), Forall({item: t2, _})) => eq(t1, t2)
  | (Forall(_), _) => false
  };
}
and tagged_eq = (t1: tagged, t2: tagged) =>
  t1.tag == t2.tag
  && (
    switch (t1.typ, t2.typ) {
    | (None, None) => true
    | (Some(t1), Some(t2)) => eq(t1, t2)
    | _ => false
    }
  );

/* Lattice join on types. This is a LUB join in the hazel2
   sense in that any type dominates Unknown. The optional
   resolve parameter specifies whether, in the case of a type
   variable and a succesful join, to return the resolved join type,
   or to return the (first) type variable for readability */
let rec join = (ty1: t, ty2: t): option(t) => {
  switch (ty1, ty2) {
  | (Unknown(p1), Unknown(p2)) =>
    Some(Unknown(join_type_provenance(p1, p2)))
  | (Unknown(_), ty)
  | (ty, Unknown(_)) => Some(ty)
  | (Rec({item: t1, ann}), Rec({item: t2, _})) =>
    switch (join(t1, t2)) {
    | Some(t) => Some(Rec({item: t, ann}))
    | None => None
    }
  | (Rec(_), _) => None
  | (Forall({item: t1, ann}), Forall({item: t2, _})) =>
    switch (join(t1, t2)) {
    | Some(t) => Some(Forall({item: t, ann}))
    | None => None
    }
  | (Forall(_), _) => None
  | (Var({item: n1, ann}), Var({item: n2, _})) =>
    if (n1 == n2) {
      Some(Var({item: n1, ann}));
    } else {
      None;
    }
  | (Var(_), _) => None
  | (Int, Int) => Some(Int)
  | (Int, _) => None
  | (Float, Float) => Some(Float)
  | (Float, _) => None
  | (Bool, Bool) => Some(Bool)
  | (Bool, _) => None
  | (String, String) => Some(String)
  | (String, _) => None
  | (Arrow(ty1_1, ty1_2), Arrow(ty2_1, ty2_2)) =>
    switch (join(ty1_1, ty2_1), join(ty1_2, ty2_2)) {
    | (Some(ty1), Some(ty2)) => Some(Arrow(ty1, ty2))
    | _ => None
    }
  | (Arrow(_), _) => None
  | (Prod(tys1), Prod(tys2)) =>
    if (List.length(tys1) != List.length(tys2)) {
      None;
    } else {
      switch (List.map2(join, tys1, tys2) |> Util.OptUtil.sequence) {
      | None => None
      | Some(tys) => Some(Prod(tys))
      };
    }
  | (Prod(_), _) => None
  | (LabelSum(tys1), LabelSum(tys2)) =>
    if (List.length(tys1) != List.length(tys2)) {
      None;
    } else {
      List.map2(tagged_join, sort_tagged(tys1), sort_tagged(tys2))
      |> Util.OptUtil.sequence
      |> Option.map(tys => LabelSum(tys));
    }
  | (LabelSum(_), _) => None
  | (Sum(ty1_1, ty1_2), Sum(ty2_1, ty2_2)) =>
    switch (join(ty1_1, ty2_1), join(ty1_2, ty2_2)) {
    | (Some(ty1), Some(ty2)) => Some(Sum(ty1, ty2))
    | _ => None
    }
  | (Sum(_), _) => None
  | (List(ty_1), List(ty_2)) =>
    switch (join(ty_1, ty_2)) {
    | Some(ty) => Some(List(ty))
    | None => None
    }
  | (List(_), _) => None
  };
}
and tagged_join = (t1: tagged, t2: tagged): option(tagged) =>
  t1.tag == t2.tag
    ? switch (t1.typ, t2.typ) {
      | (None, None) => Some({tag: t1.tag, typ: None})
      | (Some(ty1), Some(ty2)) =>
        open Util.OptUtil.Syntax;
        let+ ty_join = join(ty1, ty2);
        {tag: t1.tag, typ: Some(ty_join)};
      | _ => None
      }
    : None;

let join_all = (ts: list(t)): option(t) =>
  List.fold_left(
    (acc, ty) => Util.OptUtil.and_then(join(ty), acc),
    Some(Unknown(Internal)),
    ts,
  );

// Substitute the type variable with de bruijn index 0
let rec subst = (s: t, ~x: int=0, ty: t) => {
  let subst_keep = subst(~x, s);
  let subst_incr = subst(~x=x + 1, s);
  switch (ty) {
  | Int => Int
  | Float => Float
  | Bool => Bool
  | String => String
  | Unknown(prov) => Unknown(prov)
  | Arrow(ty1, ty2) => Arrow(subst_keep(ty1), subst_keep(ty2))
  | Prod(tys) => Prod(List.map(ty => subst_keep(ty), tys))
  | LabelSum(tys) =>
    LabelSum(
      List.map(
        ty => {tag: ty.tag, typ: Option.map(subst_keep, ty.typ)},
        tys,
      ),
    )
  | Sum(ty1, ty2) => Sum(subst_keep(ty1), subst_keep(ty2))
  | List(ty) => List(subst_keep(ty))
  | Rec({item, ann}) => Rec({item: subst_incr(item), ann})
  | Forall({item, ann}) => Forall({item: subst_incr(item), ann})
  | Var({item: y, _}) => Some(x) == y ? s : ty
  };
};

// Lookup the type variable with de bruijn index 0
let rec lookup_surface = (~x: int=0, ty: t) => {
  let lookup_keep = lookup_surface(~x);
  let lookup_incr = lookup_surface(~x=x + 1);
  switch (ty) {
  | Int
  | Float
  | Bool
  | String
  | Unknown(_) => false
  | Arrow(ty1, ty2) => lookup_keep(ty1) || lookup_keep(ty2)
  | Prod(tys) => List.exists(lookup_keep, tys)
  | LabelSum(tys) =>
    List.exists(
      ty =>
        switch (ty.typ) {
        | Some(ty) => lookup_keep(ty)
        | None => false
        },
      tys,
    )
  | Sum(ty1, ty2) => lookup_keep(ty1) || lookup_keep(ty2)
  | List(ty) => lookup_keep(ty)
  | Rec({item, _}) => lookup_incr(item)
  | Forall({item, _}) => lookup_incr(item)
  | Var({item: y, _}) => Some(x) == y
  };
};
