[@deriving (show({with_path: false}), sexp, yojson, ord)]
type t =
  | Star_
  | Seq_(Regex.s, Regex.s)
  | Alt_(Regex.s, Regex.s);

let zip = (f: t, r: Regex.t) =>
  switch (f) {
  | Star_ => Star(r)
  | Alt_(ls, rs) => Alt(List.rev(ls) @ [r, ...rs])
  | Seq_(ls, rs) => Seq(List.rev(ls) @ [r, ...rs])
  };

let nullable = (side: Dir.t, f: t) =>
  switch (side, f) {
  | (_, Star_ | Alt_(_)) => true
  | (L, Seq_(gs_l, _)) => nullable(Seq(gs_l))
  | (R, Seq_(_, gs_r)) => nullable(Seq(gs_r))
  };
