module type STATE = {type t;};

module type S = {
  type state;

  include Monads.MONAD with type t('a) = state => (state, 'a);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t('a) = state => (state, 'a);

  let get: t(state);
  let put: state => t(unit);
  let modify: (state => state) => t(unit);
  let modify': (state => ('a, state)) => t('a);
};

module Make = (ST: STATE) => {
  type state = ST.t;

  module T = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t('a) = state => (state, 'a);

    let return: ('a, state) => (state, 'a) = (x, s) => (s, x);

    let bind: (t('a), 'a => t('b)) => t('b) =
      (xf, f, s) => {
        let (s', x) = xf(s);
        f(x, s');
      };

    let get = s => (s, s);

    let put = (x, _) => (x, ());

    let modify = f => bind(get, s => put(f(s)));

    let modify' = f =>
      bind(
        get,
        s => {
          let (x, s) = f(s);
          bind(put(s), _ => return(x));
        },
      );
  };

  include T;
  include Monads.Make_Monad_B(T);
};
