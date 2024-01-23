open Sexplib.Std;

module Base = {
  // top-down
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    cells: list(int),
    token: int,
  };
  let compare = (_, _) => failwith("todo");
};
include Base;

exception Invalid;

module Map = Map.Make(Base);

let cons = (n, path) => {...path, cells: [n, ...path.cells]};

module Cursor = {
  type offset = int;
  type t = option((Base.t, offset));
  let cons = n => Option.map(((path, offset)) => (cons(n, path), offset));
  let uncons = (n, c) =>
    Option.bind(c, ((path, offset)) =>
      switch (path.cells) {
      | [m, ...cells] when m == n => Some(({...path, cells}, offset))
      | _ => None
      }
    );
  let union = (l, r) =>
    switch (l, r) {
    | (None, None) => None
    | (Some(_), _) => l
    | (_, Some(_)) => r
    };
};
module Ghosts = {
  include Map;
  type t = Map.t(Mold.t);
  let to_list = bindings;
  let of_list = bindings => of_seq(List.to_seq(bindings));
  let cons = (n, ghosts) =>
    to_list(ghosts)
    |> List.rev_map(((path, mold)) => (cons(n, path), mold))
    |> of_list;
  let uncons = (n, ghosts) =>
    to_list(ghosts)
    |> List.filter_map(((path, mold)) =>
         switch (path.cells) {
         | [m, ...cells] when m == n => Some(({...path, cells}, mold))
         | _ => None
         }
       )
    |> of_list;
  let union = union((_, m, _) => Some(m));
};
module Marks = {
  type t = {
    cursor: Cursor.t,
    ghosts: Ghosts.t,
  };
  let cons = (n, {cursor, ghosts}) => {
    cursor: Cursor.cons(n, cursor),
    ghosts: Ghosts.cons(n, ghosts),
  };
  let uncons = (n, {cursor, ghosts}) => {
    cursor: Cursor.uncons(n, cursor),
    ghosts: Ghosts.uncons(n, ghosts),
  };
  let union = (l: t, r: t) => {
    cursor: Cursor.union(l.cursor, r.cursor),
    ghosts: Ghosts.union(l.ghosts, r.ghosts),
  };
};
