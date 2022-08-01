module Ident0: {
  [@deriving sexp]
  type t;

  let v: string => t;
  let of_string: string => t;
  let to_string: t => string;

  let equal: (t, t) => bool;
  let compare: (t, t) => int;
  let length: t => int;

  let concat: (t, t) => t;
  let join: (t, t) => t;
};

include  (module type of Ident0) with type t = Ident0.t;

module Map: (module type of Util.MapSexp.Make(Ident0));
