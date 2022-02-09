open Sexplib.Std;

[@deriving sexp]
type t =
  // List ?? is it empty and non-empty?
  | Truth
  | Falsity
  | Hole
  | Int(int)
  | NotInt(int)
  | Bool(bool)
  | NotBool(bool)
  | Float(float)
  | NotFloat(float)
  | And(t, t)
  | Or(t, t)
  | InjL(t)
  | InjR(t)
  | Pair(t, t);

let rec constrains = (c: t, ty: HTyp.t): bool =>
  switch (c, ty) {
  | (Truth, _)
  | (Falsity, _)
  | (Hole, _) => true
  | (Int(_) | NotInt(_), Int) => true
  | (Int(_) | NotInt(_), _) => false
  | (Bool(_) | NotBool(_), Bool) => true
  | (Bool(_) | NotBool(_), _) => false
  | (Float(_) | NotFloat(_), Float) => true
  | (Float(_) | NotFloat(_), _) => false
  | (And(c1, c2), ty) => constrains(c1, ty) && constrains(c2, ty)
  | (Or(c1, c2), ty) => constrains(c1, ty) && constrains(c2, ty)
  | (InjL(c1), Sum(ty1, _)) => constrains(c1, ty1)
  | (InjL(_), _) => false
  | (InjR(c2), Sum(_, ty2)) => constrains(c2, ty2)
  | (InjR(_), _) => false
  | (Pair(c1, c2), ty) => constrains(c1, ty) && constrains(c2, ty)
  };

let rec dual = (c: t): t =>
  switch (c) {
  | Truth => Falsity
  | Falsity => Truth
  | Hole => Hole
  | Int(n) => NotInt(n)
  | NotInt(n) => Int(n)
  | Bool(b) => NotBool(b)
  | NotBool(b) => Bool(b)
  | Float(n) => NotFloat(n)
  | NotFloat(n) => Float(n)
  | And(c1, c2) => Or(dual(c1), dual(c2))
  | Or(c1, c2) => And(dual(c1), dual(c2))
  | InjL(c1) => Or(InjL(dual(c1)), InjR(Truth))
  | InjR(c2) => Or(InjR(dual(c2)), InjL(Truth))
  | Pair(c1, c2) =>
    Or(
      Pair(c1, dual(c2)),
      Or(Pair(dual(c1), c2), Pair(dual(c1), dual(c2))),
    )
  };

/** substitute Truth for Hole */
let rec truify = (c: t): t =>
  switch (c) {
  | Hole => Truth
  | Truth
  | Falsity
  | Int(_)
  | NotInt(_)
  | Bool(_)
  | NotBool(_)
  | Float(_)
  | NotFloat(_) => c
  | And(c1, c2) => And(truify(c1), truify(c2))
  | Or(c1, c2) => Or(truify(c1), truify(c2))
  | InjL(c) => InjL(truify(c))
  | InjR(c) => InjR(truify(c))
  | Pair(c1, c2) => Pair(truify(c1), truify(c2))
  };

/** substitute Falsity for Hole */
let rec falsify = (c: t): t =>
  switch (c) {
  | Hole => Falsity
  | Truth
  | Falsity
  | Int(_)
  | NotInt(_)
  | Bool(_)
  | NotBool(_)
  | Float(_)
  | NotFloat(_) => c
  | And(c1, c2) => And(falsify(c1), falsify(c2))
  | Or(c1, c2) => Or(falsify(c1), falsify(c2))
  | InjL(c) => InjL(falsify(c))
  | InjR(c) => InjR(falsify(c))
  | Pair(c1, c2) => Pair(falsify(c1), falsify(c2))
  };

let unwrapL =
  fun
  | InjL(c) => c
  | _ => failwith("input can only be InjL(_)");

let unwrapR =
  fun
  | InjR(c) => c
  | _ => failwith("input can only be InjR(_)");

let unwrap_pair =
  fun
  | Pair(c1, c2) => (c1, c2)
  | _ => failwith("input can only be pair(_, _)");
