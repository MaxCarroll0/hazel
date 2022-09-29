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

/* TYP.T: Hazel types */
[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Unknown(type_provenance)
  | Int
  | Float
  | Bool
  | String
  | List(t)
  | Arrow(t, t)
  | Sum(t, t) // unused
  | Prod(list(t));

/* SOURCE: Hazel type annotated with a relevant source location.
   Currently used to track match branches for inconsistent
   branches errors, but could perhaps be used more broadly
   for type debugging UI. */
[@deriving (show({with_path: false}), sexp, yojson)]
type source = {
  id: int,
  ty: t,
};

/* SELF: The (synthetic) type information derivable from a term
   in isolation, using the typing context but not the syntactic
   context. This can either be Free (no type, in the case of
   free variables), Joined (a list of types, possibly
   inconsistent, generated by branching forms like ifs,
   matches, and list literals), or Just a regular type. */
[@deriving (show({with_path: false}), sexp, yojson)]
type self =
  | Just(t)
  // TODO: make it so that joined applies only to inconsistent types; rename NoJoin
  | Joined(t => t, list(source))
  | Multi
  | Free;

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

/* Lattice join on types. This is a LUB join in the hazel2
   sense in that any type dominates Unknown */
let rec join = (ty1: t, ty2: t): option(t) =>
  switch (ty1, ty2) {
  | (Unknown(p1), Unknown(p2)) =>
    Some(Unknown(join_type_provenance(p1, p2)))
  | (Unknown(_), ty)
  | (ty, Unknown(_)) => Some(ty)
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

let join_all: list(t) => option(t) =
  List.fold_left(
    (acc, ty) => Util.OptUtil.and_then(join(ty), acc),
    Some(Unknown(Internal)),
  );

let join_or_fst = (ty: t, ty': t): t =>
  switch (join(ty, ty')) {
  | None => ty
  | Some(ty) => ty
  };

let t_of_self =
  fun
  | Just(t) => t
  | Joined(wrap, ss) =>
    switch (ss |> List.map(s => s.ty) |> join_all) {
    | None => Unknown(Internal)
    | Some(t) => wrap(t)
    }
  | Multi
  | Free => Unknown(Internal);

/* MATCHED JUDGEMENTS: Note that matched judgements work
   a bit different than hazel2 here since hole fixing is
   implicit. Somebody should check that what I'm doing
   here actually makes sense -Andrew */

let matched_arrow: t => (t, t) =
  fun
  | Arrow(ty_in, ty_out) => (ty_in, ty_out)
  | Unknown(prov) => (Unknown(prov), Unknown(prov))
  | _ => (Unknown(Internal), Unknown(Internal));

let matched_arrow_mode: mode => (mode, mode) =
  fun
  | SynFun
  | Syn => (Syn, Syn)
  | Ana(ty) => {
      let (ty_in, ty_out) = matched_arrow(ty);
      (Ana(ty_in), Ana(ty_out));
    };

let matched_prod_mode = (mode: mode, length): list(mode) =>
  switch (mode) {
  | Ana(Prod(ana_tys)) when List.length(ana_tys) == length =>
    List.map(ty => Ana(ty), ana_tys)
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

let matched_list_lit_mode = (mode: mode, length): list(mode) =>
  switch (mode) {
  | SynFun
  | Syn => List.init(length, _ => Syn)
  | Ana(ty) => List.init(length, _ => Ana(matched_list(ty)))
  };

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
  | Prod([])
  | List(_) => precedence_const
  | Prod(_) => precedence_Prod
  | Sum(_, _) => precedence_Sum
  | Arrow(_, _) => precedence_Arrow
  };

/* equality
   At the moment, this coincides with default equality,
   but this will change when polymorphic types are implemented */
let rec eq = (t1, t2) =>
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
  };
