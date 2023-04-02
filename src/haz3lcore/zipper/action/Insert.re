open Zipper;
open Util;
open OptUtil.Syntax;

[@deriving (show({with_path: false}), sexp, yojson)]
type appendability =
  | AppendLeft(Token.t)
  | AppendRight(Token.t)
  | MakeNew;

let sibling_appendability: (string, Siblings.t) => appendability =
  (char, siblings) =>
    switch (neighbor_monotiles(siblings)) {
    | (Some(t), _) when Form.is_valid_token(t ++ char) =>
      AppendLeft(t ++ char)
    | (_, Some(t))
        when Form.is_valid_token(char ++ t) && !Form.is_comment_delim(char) =>
      AppendRight(char ++ t)
    | _ => MakeNew
    };

let expand_keyword = ((z, _) as state: state): option(state) =>
  /* NOTE(andrew): We may want to allow editing of shards when only 1 of set
     is down (removing the rest of the set from backpack on edit) as something
     like this is necessary for backspace to act as undo after kw-expansion */
  switch (neighbor_monotiles(z.relatives.siblings)) {
  | (Some(kw), _) =>
    let (new_label, direction) = Molds.delayed_completion(kw, Left);
    Zipper.replace(direction, new_label, state);
  | _ => Some(state)
  };

let get_duo_shard = ({label, shards, _}: Tile.t) =>
  if (List.length(label) == 2 && List.length(shards) == 1) {
    List.nth_opt(label, List.hd(shards));
  } else {
    None;
  };

let neighbor_can_duomerge =
    (t: Token.t, s: Siblings.t): option((Label.t, Direction.t)) =>
  switch (Siblings.neighbors(s)) {
  | (Some(Tile(tile)), _) =>
    let* start = get_duo_shard(tile);
    let+ mono_lbl = Form.duomerges([start, t]);
    (mono_lbl, Direction.Left);
  | (_, Some(Tile(tile))) =>
    let* last = get_duo_shard(tile);
    let+ mono_lbl = Form.duomerges([t, last]);
    (mono_lbl, Direction.Right);
  | _ => None
  };

let replace_neighbor = (lbl: Label.t, d: Direction.t, z: t, id_gen) =>
  //TODO(andrew): not sure why i have to select twice here...
  z
  |> Zipper.select(d)
  |> OptUtil.and_then(Zipper.select(d))
  |> Option.map(Zipper.destruct)
  |> Option.get  //TODO(andrew): error handling
  |> (z => Zipper.construct(d, lbl, z, id_gen));

let barf_or_construct =
    (t: Token.t, direction_pref: Direction.t, z: t): IdGen.t(t) => {
  let barfed =
    Backpack.is_first_matching(t, z.backpack) ? Zipper.put_down(z) : None;
  switch (barfed, neighbor_can_duomerge(t, z.relatives.siblings)) {
  | (Some(z), Some((lbl, d))) => replace_neighbor(lbl, d, z)
  | (Some(z), _) => IdGen.return(z)
  | (None, _) =>
    let (lbl, direction) = Molds.instant_completion(t, direction_pref);
    Zipper.construct(direction, lbl, z);
  };
};

let expand_and_barf_or_construct = (char: string, state: state) =>
  state
  |> expand_keyword
  |> Option.map(((z, id_gen)) => barf_or_construct(char, Left, z, id_gen));

let insert_outer = (char: string, (z, id_gen): state): option(state) =>
  switch (sibling_appendability(char, z.relatives.siblings)) {
  | MakeNew => expand_neighbors_and_make_new_tile(char, state)
  | AppendLeft(t) => replace_tile(t, Left, state)
  | AppendRight(t) => replace_tile(t, Right, state)
  };

let insert_duo = (id_gen, lbl: Label.t, z: option(t)): option(state) =>
  z
  |> Option.map(z => Zipper.construct(Left, lbl, z, id_gen))
  |> OptUtil.and_then(((z, id_gen)) =>
       Zipper.put_down(z)
       |> OptUtil.and_then(Zipper.move(Left))
       |> Option.map(z => (z, id_gen))
     );

let insert_monos =
    (id_gen, l: Token.t, r: Token.t, z: option(t)): option(state) =>
  z
  |> Option.map(z => Zipper.construct(Left, [l], z, id_gen))
  |> Option.map(((z, id_gen)) => Zipper.construct(Right, [r], z, id_gen));

let split =
    ((z, id_gen): state, char: string, idx: int, t: Token.t): option(state) => {
  let (l, r) = Token.split_nth(idx, t);
  z
  |> Zipper.set_caret(Outer)
  |> Zipper.select(Right)
  |> (
    // overwrite
    switch (Form.duomerges([l, r])) {
    | Some(_) => insert_duo(id_gen, [l, r])
    | None => insert_monos(id_gen, l, r)
    }
  )
  |> OptUtil.and_then(expand_and_barf_or_construct(char));
};

let opt_regrold = d =>
  Option.map(((z, id_gen)) => remold_regrout(d, z, id_gen));

let move_into_if_stringlit = (char, z) =>
  /* This is special-case logic for advancing the caret to position between the quotes
     in newly-created stringlits. The main stringlit special-case is in Zipper.constuct
     and ideally this logic would be located there as well, but both regrouting and
     subsequent caret position logic at this function's callsites dicate that this
     be done after. Not too happy about this tbh. */
  Form.is_string_delim(char)
    ? switch (move(Left, z)) {
      | None => z
      | Some(z) => z |> set_caret(Inner(0, 0))
      }
    : z;

let go =
    (
      char: string,
      ({caret, relatives: {siblings, _}, _} as z, id_gen): state,
    )
    : option(state) => {
  /* If there's a selection, delete it before proceeding */
  let z = z.selection.content != [] ? Zipper.destruct(z) : z;
  switch (caret, neighbor_monotiles(siblings)) {
  /* Special cases for insertion of quotes when the caret is
     in or is adjacent to a string/comment. This is necessary to
     avoid breaking paste. */
  | (_, (_, Some(t)))
      when
        Form.is_string(t)
        && Form.is_string_delim(char)
        || Form.is_comment(t)
        && Form.is_comment_delim(char) =>
    z
    |> Zipper.set_caret(Outer)
    |> Zipper.move(Right)
    |> Option.map(z => (z, id_gen))
  | (Outer, (Some(t), _))
      when
        Form.is_string(t)
        && Form.is_string_delim(char)
        || Form.is_comment(t)
        && Form.is_comment_delim(char) =>
    Some((z, id_gen))
  | (Inner(d_idx, n), (_, Some(t))) =>
    let idx = n + 1;
    let new_t = Token.insert_nth(idx, char, t);
    /* If inserting wouldn't produce a valid token, split */
    Form.is_valid_token(new_t)
      ? z
        |> Zipper.set_caret(Inner(d_idx, idx))
        |> (z => Zipper.replace_mono(Right, new_t, (z, id_gen)))
        |> opt_regrold(Left)
      : split((z, id_gen), char, idx, t) |> opt_regrold(Right);
  /* Can't insert inside delimiter */
  | (Inner(_, _), (_, None)) => None
  | (Outer, (_, Some(_))) =>
    let caret: Zipper.Caret.t =
      /* If we're adding to the right, move caret inside right nhbr */
      switch (sibling_appendability(char, siblings)) {
      | AppendRight(_) => Inner(0, 0) //Note: assumption of monotile
      | MakeNew
      | AppendLeft(_) => Outer
      };
    (z, id_gen)
    |> insert_outer(char)
    |> Option.map(((z, id_gen)) => (Zipper.set_caret(caret, z), id_gen))
    |> opt_regrold(Left)
    |> Option.map(((z, id_gen)) =>
         (move_into_if_stringlit(char, z), id_gen)
       );
  | (Outer, (_, None)) =>
    insert_outer(char, (z, id_gen))
    |> opt_regrold(Left)
    |> Option.map(((z, id_gen)) =>
         (move_into_if_stringlit(char, z), id_gen)
       )
  };
};
