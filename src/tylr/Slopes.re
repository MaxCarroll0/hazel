open Util;
open Slope;

[@deriving (show({with_path: false}), sexp, yojson)]
type t('a) = (Dn.t('a), Up.t('a));
[@deriving (show({with_path: false}), sexp, yojson)]
type p = t(Piece.t);

let mk = (~l=Slope.empty, ~r=Slope.empty, ()) => (l, r);
let empty = mk();

let cons = (~onto_l, ~onto_r, ~onto: Dir.t, a, (l, r): t) =>
  switch (onto) {
  | L => (onto_l(l, a), r)
  | R => (l, onto_r(a, r))
  };
let cons_space = cons(~onto_l=Dn.snoc_space, ~onto_r=Up.cons_space);

let uncons_lexeme = (~char=false, ~from: Dir.t, (l, r)) =>
  switch (from) {
  | L =>
    Dn.unsnoc_lexeme(~char, l) |> Option.map(((l, lx)) => (lx, (l, r)))
  | R =>
    Up.uncons_lexeme(~char, r) |> Option.map(((lx, r)) => (lx, (l, r)))
  };
let uncons_opt_lexeme = (~char=false, ~from: Dir.t, sib) =>
  switch (uncons_lexeme(~char, ~from, sib)) {
  | None => (None, sib)
  | Some((lx, sib)) => (Some(lx), sib)
  };
let uncons_opt_lexemes = (~char=false, sib: t) => {
  let (l, sib) = uncons_opt_lexeme(~char, ~from=L, sib);
  let (r, sib) = uncons_opt_lexeme(~char, ~from=R, sib);
  ((l, r), sib);
};
let peek_lexemes = sib => fst(uncons_opt_lexemes(sib));

let cat = ((l_inner, r_inner), (l_outer, r_outer)) => (
  Dn.cat(l_outer, l_inner),
  Up.cat(r_inner, r_outer),
);
// let concat = _ => failwith("todo concat");

let bounds = ((l, r): t) => ListUtil.(hd_opt(l.terrs), hd_opt(r.terrs));

// let peek_space = ((l, r): t) => (Chain.lst(l), Chain.fst(r));

// let bound = (~l=None, ~r=None, (pre, suf): t) =>
//   Segment.Bounded.(bound_l(l, pre), bound_r(suf, r));

// let offset = sib =>
//   switch (peek_lexemes(sib)) {
//   | (Some(l), Some(r)) when Lexeme.(id(l) == id(r)) => - Lexeme.length(r)
//   | _ => List.length(fst(peek_space(sib)))
//   };
// let steps = ((l, _): t) => List.map(Meld.length, Segment.melds(l));
// let path = (sib: t) =>
//   Meld.Path.mk(~offset=offset(sib), ~steps=steps(sib), ());

let rec zip = (slopes: t, kid: Meld.p) => {
  // let kid = Meld.pad(~l=dn.space, kid, ~r=up.space);
  switch (slopes) {
  | ([], _) => Up.zip(kid, up)
  | (_, []) => Dn.zip(dn, kid)
  | ([l, ...tl_l], [r, ...tl_r]) =>
    switch (Terrace.cmp(l, ~slot, r)) {
    | None =>
      print_endline("l = " ++ Terrace.show(l));
      print_endline("r = " ++ Terrace.show(r));
      raise(Meld.Invalid_prec);
    | Some(Lt(kid_r)) => zip((dn, Up.mk(tl_r)), kid_r)
    | Some(Gt(l_kid)) => zip((Dn.mk(tl_l), up), l_kid)
    | Some(Eq(l_kid_r)) => zip((Dn.mk(tl_l), Up.mk(tl_r)), l_kid_r)
    }
  };
};
let zip_init = ((dn, up): t) =>
  switch (dn.terrs, up.terrs) {
  // when caret is in the middle of a piece
  | ([l, ...terrs_l], [r, ...terrs_r])
      when
        Space.(is_empty(dn.space) && is_empty(up.space))
        && Piece.zips(Terrace.R.face(l), Terrace.L.face(r)) =>
    let (tl_l, hd_l) = Terrace.R.split_face(l);
    let (hd_r, tl_r) = Terrace.L.split_face(r);
    let p = Option.get(Piece.zip(hd_l, hd_r));
    let kid = Meld.append(tl_l, p, tl_r);
    zip((Dn.mk(terrs_l), Up.mk(terrs_r)), kid);
  | _ => zip((dn, up), Meld.empty())
  };
