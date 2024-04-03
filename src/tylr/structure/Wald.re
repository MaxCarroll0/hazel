open Util;

include Meld.Wald;
[@deriving (show({with_path: false}), sexp, yojson)]
type t = Meld.Wald.t(Cell.t);

let split_hd = (W(w)) => Chain.split_fst(w);
let hd = w => fst(split_hd(w));

// let fst = (W(w)) => Chain.fst(w);
// let lst = (W(w)) => Chain.lst(w);
// let face =
//   fun
//   | Dir.L => fst
//   | R => lst;

let get = (f, W(w)) => f(w);
let map = (f, W(w)) => W(f(w));

let length: t => int = get(Chain.length);
let rev: t => t = map(Chain.rev);

let link = (tok, cell) => map(Chain.link(tok, cell));
let unlink = (W(w)) =>
  Chain.unlink(w)
  |> Result.map(~f=((tok, cell, tl)) => (tok, cell, W(tl)));

let cells: t => list(Cell.t) = get(Chain.links);

let extend = tl => map(Chain.extend(tl));

let sort = w => Token.sort(hd(w));

let fold = (f, g, W(w)) => Chain.fold_left(f, g, w);

module Tl = {
  include Chain.Tl;
  type t = Chain.Tl.t(Cell.t, Token.t);
};

let unzip_tok = (n, W(w)) => Chain.unzip_nth(n, w);
let zip_tok = (~pre=Tl.empty, ~suf=Tl.empty, tok) =>
  W(Chain.zip(~pre, tok, ~suf));

let unzip_cell = (n, W((toks, cells)): t) => {
  let (tok, (toks_l, toks_r)) = ListUtil.split_frame(n, toks);
  let (cell, (cs_l, cs_r)) = ListUtil.split_frame(n, cells);
  (mk([tok, ...toks_l], cs_l), cell, mk(toks_r, cs_r));
};
let zip_cell = (pre: t, cell: Cell.t, suf: t) =>
  pre
  |> fold(
       tok => link(tok, cell, suf),
       (zipped, cell, tok) => link(tok, cell, zipped),
     );

let zip_hds = (~from: Dir.t, src: t, dst: t): option(t) => {
  let (hd_src, tl_src) = split_hd(src);
  let (hd_dst, tl_dst) = split_hd(dst);
  let (hd_l, hd_r) = Dir.order(from, (hd_src, hd_dst));
  Token.merge(hd_l, hd_r)
  |> Option.map(tok => W(Chain.zip(~pre=tl_dst, tok, ~suf=tl_src)));
};
