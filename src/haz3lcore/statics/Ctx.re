open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type value =
  | Typ(Typ.t)
  | Kind(Kind.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type entry = {
  id: Id.t,
  value,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = VarMap.t_(entry);

[@deriving (show({with_path: false}), sexp, yojson)]
type co_item = {
  id: Id.t,
  mode: Typ.mode,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type co_entry = list(co_item);

[@deriving (show({with_path: false}), sexp, yojson)]
type co = VarMap.t_(co_entry);

let empty = VarMap.empty;

let lookup_typ = (ctx: t, x) =>
  VarMap.find_map(
    ((k, {id: _, value})) =>
      switch (value) {
      | Typ(t) =>
        if (k == x) {
          Some(t);
        } else {
          None;
        }
      | _ => None
      },
    ctx,
  );

let subtract_typ = (ctx: t, free: co): co =>
  VarMap.filter(
    ((k, _)) =>
      switch (lookup_typ(ctx, k)) {
      | None => true
      | Some(_) => false
      },
    free,
  );

let subtract_prefix = (ctx: t, prefix_ctx: t): option(t) => {
  // NOTE: does not check that the prefix is an actual prefix
  let prefix_length = List.length(prefix_ctx);
  let ctx_length = List.length(ctx);
  if (prefix_length > ctx_length) {
    None;
  } else {
    Some(
      List.rev(
        Util.ListUtil.sublist((prefix_length, ctx_length), List.rev(ctx)),
      ),
    );
  };
};

//TODO(andrew): is this correct in the case of duplicates?
let union: list(co) => co =
  List.fold_left((free1, free2) => free1 @ free2, []);

module VarSet = Set.Make(Token);

// Note: filter out duplicates when rendering
let filter_duplicates = (ctx: t): t =>
  ctx
  |> List.fold_left(
       ((ctx, term_set, typ_set), (x, entry)) => {
         switch (entry.value) {
         | Typ(_) =>
           VarSet.mem(x, term_set)
             ? (ctx, term_set, typ_set)
             : (
               VarMap.extend(ctx, (x, entry)),
               VarSet.add(x, term_set),
               typ_set,
             )
         | Kind(_) =>
           VarSet.mem(x, term_set)
             ? (ctx, term_set, typ_set)
             : (
               VarMap.extend(ctx, (x, entry)),
               term_set,
               VarSet.add(x, typ_set),
             )
         }
       },
       (VarMap.empty, VarSet.empty, VarSet.empty),
     )
  |> (((ctx, _, _)) => List.rev(ctx));
