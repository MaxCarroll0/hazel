open Haz3lcore;
open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type syntax_result = {
  results: list(bool),
  percentage: float,
};

let rec find_var_upat = (name: string, upat: Term.UPat.t): bool => {
  switch (upat.term) {
  | Var(x) => x == name
  | Cons(up1, up2) => find_var_upat(name, up1) || find_var_upat(name, up2)
  | Tuple(l) =>
    List.fold_left((acc, up) => {acc || find_var_upat(name, up)}, false, l)
  | Parens(up) => find_var_upat(name, up)
  | Ap(up1, up2) => find_var_upat(name, up1) || find_var_upat(name, up2)
  | TypeAnn(up, _) => find_var_upat(name, up)
  | ListLit(l) =>
    List.fold_left((acc, up) => {acc || find_var_upat(name, up)}, false, l)
  | _ => false
  };
};

let rec var_mention = (name: string, uexp: Term.UExp.t): bool => {
  switch (uexp.term) {
  | Var(x) => x == name
  | Fun(args, body) =>
    if (find_var_upat(name, args)) {
      false;
    } else {
      var_mention(name, body);
    }
  | Tuple(l) =>
    List.fold_left((acc, ue) => {acc || var_mention(name, ue)}, false, l)
  | Let(p, def, body) =>
    if (find_var_upat(name, p)) {
      false;
    } else {
      var_mention(name, def) || var_mention(name, body);
    }
  | Ap(u1, u2) => var_mention(name, u1) || var_mention(name, u2)
  | If(u1, u2, u3) =>
    var_mention(name, u1) || var_mention(name, u2) || var_mention(name, u3)
  | Seq(u1, u2) => var_mention(name, u1) || var_mention(name, u2)
  | Test(u) => var_mention(name, u)
  | Parens(u) => var_mention(name, u)
  | Cons(u1, u2) => var_mention(name, u1) || var_mention(name, u2)
  | UnOp(_, u) => var_mention(name, u)
  | BinOp(_, u1, u2) => var_mention(name, u1) || var_mention(name, u2)
  | Match(g, l) =>
    var_mention(name, g)
    || List.fold_left(
         (acc, pe) => {
           let (p, e) = pe;
           if (find_var_upat(name, p)) {
             false;
           } else {
             acc || var_mention(name, e);
           };
         },
         false,
         l,
       )
  | ListLit(l) =>
    List.fold_left((acc, ue) => {acc || var_mention(name, ue)}, false, l)

  | _ => false
  };
};

let rec find_in_let =
        (
          name: string,
          upat: Term.UPat.t,
          def: Term.UExp.t,
          l: list(Term.UExp.t),
        )
        : list(Term.UExp.t) => {
  switch (upat.term, def.term) {
  | (Parens(up), Parens(ue)) => find_in_let(name, up, ue, l)
  | (Parens(up), _) => find_in_let(name, up, def, l)
  | (_, Parens(ue)) => find_in_let(name, upat, ue, l)
  | (TypeAnn(up, _), _) => find_in_let(name, up, def, l)
  | (Var(x), Fun(_)) =>
    if (x == name) {
      [def, ...l];
    } else {
      l;
    }
  | (Tuple(pl), Tuple(ul)) =>
    if (List.length(pl) != List.length(ul)) {
      l;
    } else {
      List.fold_left2(
        (acc, up, ue) => {find_in_let(name, up, ue, acc)},
        l,
        pl,
        ul,
      );
    }
  | _ => l
  };
};

let rec find_fn =
        (name: string, uexp: Term.UExp.t, l: list(Term.UExp.t))
        : list(Term.UExp.t) => {
  switch (uexp.term) {
  | Let(up, def, body) =>
    l |> find_in_let(name, up, def) |> find_fn(name, body)
  | ListLit(ul) =>
    List.fold_left((acc, u1) => {find_fn(name, u1, acc)}, l, ul)
  | Tuple(ul) =>
    List.fold_left((acc, u1) => {find_fn(name, u1, acc)}, l, ul)
  | Fun(_, body) => l |> find_fn(name, body)
  | Ap(u1, u2) => l |> find_fn(name, u1) |> find_fn(name, u2)
  | If(u1, u2, u3) =>
    l |> find_fn(name, u1) |> find_fn(name, u2) |> find_fn(name, u3)
  | Seq(u1, u2) => l |> find_fn(name, u1) |> find_fn(name, u2)
  | Parens(u1) => l |> find_fn(name, u1)
  | Cons(u1, u2) => l |> find_fn(name, u1) |> find_fn(name, u2)
  | UnOp(_, u1) => l |> find_fn(name, u1)
  | BinOp(_, u1, u2) => l |> find_fn(name, u1) |> find_fn(name, u2)
  | Match(u1, ul) =>
    List.fold_left(
      (acc, (_, ue)) => {find_fn(name, ue, acc)},
      l |> find_fn(name, u1),
      ul,
    )
  | _ => l
  };
};

let is_recursive = (name: string, uexp: Term.UExp.t): bool => {
  let fn_bodies = [] |> find_fn(name, uexp);
  if (List.length(fn_bodies) == 0) {
    false;
  } else {
    List.fold_left(
      (acc, ue) => {acc && var_mention(name, ue)},
      true,
      fn_bodies,
    );
  };
};

/*let rec is_tail_recursive = (uexp: Term.UExp.t, name: string): bool => {
  switch (uexp.term) {
  | Fun(args, body) =>
    find_var_upat(args, name) ? false : is_tail_recursive(body, name)
  | Let(p, def, body) =>
    find_var_upat(p, name) || is_recursive(def, name)
      ? false : is_tail_recursive(body, name)
  | Tuple(l) =>
    //If l has no recursive calls then true
    !List.fold_left((acc, ue) => {acc || is_recursive(ue, name)}, false, l)
  | Ap(u1, u2) =>
    is_recursive(u2, name) ? false : is_tail_recursive(u1, name)
  | If(u1, u2, u3) =>
    is_recursive(u1, name)
      ? false : is_tail_recursive(u2, name) && is_tail_recursive(u3, name)
  | Seq(u1, u2) =>
    is_recursive(u1, name) ? false : is_tail_recursive(u2, name)
  | Test(_) => false
  | Parens(u) => is_tail_recursive(u, name)
  | Cons(u1, u2) => !(is_recursive(u1, name) || is_recursive(u2, name))
  | UnOp(_, u) => !is_recursive(u, name)
  | BinOp(_, u1, u2) => !(is_recursive(u1, name) || is_recursive(u2, name))
  | Match(g, l) =>
    is_recursive(g, name)
      ? false
      : List.fold_left(
          (acc, (p, e)) => {
            find_var_upat(p, name)
              ? false : acc && is_tail_recursive(e, name)
          },
          false,
          l,
        )
  | ListLit(l) =>
    !List.fold_left((acc, ue) => {acc || is_recursive(ue, name)}, false, l)

  | _ => true
  };
};*/

let check =
    (uexp: Term.UExp.t, predicates: list(Term.UExp.t => bool)): syntax_result => {
  let results = List.map(pred => {uexp |> pred}, predicates);
  let length = List.length(predicates);
  let passing = Util.ListUtil.count_pred(res => res, results);

  {
    results,
    percentage:
      //vacuously passes if there are no tests
      length == 0 ? 1. : float_of_int(passing) /. float_of_int(length),
  };
};
