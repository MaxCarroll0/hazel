module Base = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t('mtrl) = {
    mtrl: 'mtrl,
    mold: Mold.t,
  };
};
include Base;

let map = (f, m) => {...m, mtrl: f(m)};

module Label = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t = Base.t(Mtrl.Label.t);

  let space = {
    mtrl: Mtrl.Space,
    mold: {
      sort: Space,
      prec: 0,
      rctx: [Seq_([Atom(NT(Space))], [Atom(NT(Space))])],
    },
  };
};

module Sort = {
  [@deriving (show({with_path: false}), sexp, yojson, ord)]
  type t = Base.t(Mtrl.Sort.t);
  let bounds = _ => failwith("todo Molded.Sort.bounds");
};
