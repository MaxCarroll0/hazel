open Sexplib.Std;
open Util;

// todo: document potential same-id token on either side of caret
// l|et x = 1 in x + 1
[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  sel: Selection.t,
  rel: Valley.t,
};

let mk = (~sel=Selection.empty, rel) => {sel, rel};

let init =
  mk(
    Valley.of_sib(
      Vee.mk(
        ~r=
          Slope.Up.of_meld(
            Meld.of_grout(Grout.mk(Mold.mk_operand(Some(Sort.root)))),
          ),
        (),
      ),
    ),
  );

// let unselect = (d: Dir.t, {sel, rel}: t) =>
//   rel |> Valley.cons_seg(~onto=Dir.toggle(d), sel.seg) |> mk;
let unselect = (d: Dir.t, {sel, rel}: t) =>
  rel |> Valley.cons_arch(~onto=Dir.toggle(d), sel.arch) |> mk;

let zip = (~d=Dir.L, z: t): Meld.Zipped.t => {
  let z = unselect(d, z);
  Valley.zip(z.rel);
};

module Action = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Move(Dir.t)
    | Select(Dir.t)
    | Delete(Dir.t)
    | Insert(string);
};

let move = (d: Dir.t, z: t): option(t) =>
  if (!Selection.is_empty(z.sel)) {
    Some(unselect(d, z));
  } else {
    Valley.shift_char(~from=d, z.rel)
    |> Option.map(rel => {...z, rel: Valley.assemble(rel)});
  };

let select = (d: Dir.t, z: t): option(t) => {
  open OptUtil.Syntax;
  let b = Dir.toggle(d);
  if (d == z.sel.foc || Selection.is_empty(z.sel)) {
    let+ (c, rel) = Valley.uncons_char(~from=d, z.rel);
    let bs = Valley.bounds(rel);
    let sel = Selection.cons_lexeme(c, {...z.sel, foc: d}, bs);
    mk(~sel, rel);
  } else {
    // checked for selection empty above
    let (c, sel) = Option.get(Selection.uncons_char(z.sel));
    let rel =
      z.rel
      |> Valley.cons_lexeme(~onto=b, c)
      |> Valley.assemble(~sel=sel.seg);
    return(mk(~sel, rel));
  };
};

let delete = (d: Dir.t, z: t): option(t) => {
  open OptUtil.Syntax;
  let+ z = Selection.is_empty(z.sel) ? select(d, z) : return(z);
  let (lexed, rel) = z.rel |> Valley.assemble |> Valley.relex;
  mk(Valley.insert(lexed, rel));
};

let insert = (s: string, z: t): t => {
  let (lexed, rel) = z.rel |> Valley.assemble |> Valley.relex(~insert=s);
  mk(Valley.insert(lexed, rel));
};

let perform = (a: Action.t, z: t): option(t) =>
  switch (a) {
  | Move(d) => move(d, z)
  | Select(d) => select(d, z)
  | Delete(d) => delete(d, z)
  | Insert(s) => Some(insert(s, z))
  };
