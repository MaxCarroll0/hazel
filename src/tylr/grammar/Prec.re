open Sexplib.Std;
open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = int;
type p = t;

let compare = Int.compare;

let min = Int.min_int;

let max = Int.max_int;
let max_op = max - 1;

let lt = (~a=None, l, r) =>
  compare(l, r) < 0 || compare(l, r) == 0 && a == Some(Dir.R);
let gt = (~a=None, l, r) =>
  compare(l, r) > 0 || compare(l, r) == 0 && a == Some(Dir.L);
let eq = (~a=None, l, r) =>
  compare(l, r) == 0 && Option.is_none(a);

let leq = (~a=None, l, r) => lt(~a, l, r) || eq(~a, l, r);
let geq = (~a=None, l, r) => gt(~a, l, r) || eq(~a, l, r);

let lower_bounded = (~a=None, ~side: Dir.t, p, bound) =>
  switch (side) {
  | L => leq(~a?, bound, p)
  | R => geq(~a?, p, bound)
  };

module Map = Map.Make({
  let nonrec t = t;
  let compare = compare;
});

module Table = {
  include Map;
  type t('v) = Map.t(('v, Assoc.t));
  let of_list = failwith("todo of_list");
};

module Frame = {
  type t = (option(p), option(p));
};

module Framed = {
  type t('x) = {
    frame: Frame.t,
    subj: 'x,
  };
};

module Bounds = {
  type t = (p, p);
  let init = (min, min);
  let push = (f: Frame.t, (l, r)) =>
    switch (f) {
    | Eq(p) => Some((p, p))
    | Lt(p) when l < p => Some((l, p))
    | Gt(p) when p > r => Some((p, r))
    | _ => None
    };
};

module Bounded = {
  type t('x) = {
    bounds: Bounds.t,
    subj: 'x,
  };
};