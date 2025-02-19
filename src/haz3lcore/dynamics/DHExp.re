/* DHExp.re

   This module is specifically for dynamic expressions. They are stored
   using the same data structure as user expressions, have been modified
   slightly as described in Elaborator.re.
   */

include Exp;

let term_of: t => term = IdTagged.term_of;
let fast_copy: (Id.t, t) => t = IdTagged.fast_copy;

let mk = (ids, term): t => {
  {ids, copied: true, term};
};

// TODO: make this function emit a map of changes
let repair_ids =
  map_term(
    ~f_exp=
      (continue, exp) =>
        if (exp.copied) {
          replace_all_ids(exp);
        } else {
          continue(exp);
        },
    ~f_typ=
      (continue, typ) =>
        if (Typ.rep_id(typ) == Id.invalid) {
          replace_all_ids_typ(typ);
        } else {
          continue(typ);
        },
    _,
  );

let repair_ids_typ =
  Typ.map_term(
    ~f_exp=
      (continue, exp) =>
        if (Exp.rep_id(exp) == Id.invalid) {
          replace_all_ids(exp);
        } else {
          continue(exp);
        },
    ~f_typ=
      (continue, typ) =>
        if (typ.copied) {
          replace_all_ids_typ(typ);
        } else {
          continue(typ);
        },
    _,
  );

// Also strips static error holes - kinda like unelaboration
let rec strip_casts =
  map_term(
    ~f_exp=
      (continue, exp) => {
        switch (term_of(exp)) {
        /* Leave non-casts unchanged */
        | Tuple(_)
        | Cons(_)
        | ListConcat(_)
        | ListLit(_)
        | MultiHole(_)
        | Seq(_)
        | Filter(_)
        | Let(_)
        | FixF(_)
        | TyAlias(_)
        | Fun(_)
        | Ap(_)
        | Deferral(_)
        | DeferredAp(_)
        | Test(_)
        | BuiltinFun(_)
        | UnOp(_)
        | BinOp(_)
        | Match(_)
        | Parens(_)
        | EmptyHole
        | Invalid(_)
        | Var(_)
        | Bool(_)
        | Int(_)
        | Float(_)
        | String(_)
        | Constructor(_)
        | DynamicErrorHole(_)
        | Closure(_)
        | TypFun(_)
        | TypAp(_)
        | Undefined
        | If(_) => continue(exp)
        /* Remove casts*/
        | Cast(d, _, _) => strip_casts(d)
        /* Keep failed casts*/
        | FailedCast(_, _, _) => continue(exp)
        }
      },
    _,
  );

let assign_name_if_none = (t, name) => {
  let (term, rewrap) = unwrap(t);
  switch (term) {
  | Fun(arg, body, typ, None) => Fun(arg, body, typ, name) |> rewrap
  | TypFun(utpat, body, None) => TypFun(utpat, body, name) |> rewrap
  | _ => t
  };
};

let ty_subst = (s: Typ.t, tpat: TPat.t, exp: t): t => {
  switch (TPat.tyvar_of_utpat(tpat)) {
  | None => exp
  | Some(x) =>
    Exp.map_term(
      ~f_typ=(_, typ) => Typ.subst(s, tpat, typ),
      ~f_exp=
        (continue, exp) =>
          switch (term_of(exp)) {
          | TypFun(utpat, _, _) =>
            switch (TPat.tyvar_of_utpat(utpat)) {
            | Some(x') when x == x' => exp
            | Some(_)
            | None => continue(exp)
            /* Note that we do not have to worry about capture avoidance, since s will always be closed. */
            }
          | Cast(_)
          | FixF(_)
          | Fun(_)
          | TypAp(_)
          | ListLit(_)
          | Test(_)
          | Closure(_)
          | Seq(_)
          | Let(_)
          | Ap(_)
          | BuiltinFun(_)
          | BinOp(_)
          | Cons(_)
          | ListConcat(_)
          | Tuple(_)
          | Match(_)
          | DynamicErrorHole(_)
          | Filter(_)
          | If(_)
          | EmptyHole
          | Invalid(_)
          | Undefined
          | Constructor(_)
          | Var(_)
          | Bool(_)
          | Int(_)
          | Float(_)
          | String(_)
          | FailedCast(_, _, _)
          | MultiHole(_)
          | Deferral(_)
          | TyAlias(_)
          | DeferredAp(_)
          | Parens(_)
          | UnOp(_) => continue(exp)
          },
      exp,
    )
  };
};
