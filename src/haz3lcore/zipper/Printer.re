open Util;
open Util.OptUtil.Syntax;

[@deriving (show({with_path: false}), yojson)]
type t = {
  code: list(string),
  selection: list(string),
  backpack: list(list(string)),
};

let seg_of_zip = Zipper.seg_without_buffer;

let rec of_segment = (~holes, seg: Segment.t): string =>
  seg |> List.map(of_piece(~holes)) |> String.concat("")
and of_piece = (~holes, p: Piece.t): string =>
  switch (p) {
  | Tile(t) => of_tile(~holes, t)
  | Grout({shape: Concave, _}) => " "
  | Grout({shape: Convex, _}) when holes != None => Option.get(holes)
  | Grout({shape: Convex, _}) => " "
  | Secondary(w) =>
    Secondary.is_linebreak(w) ? "\n" : Secondary.get_string(w.content)
  }
and of_tile = (~holes, t: Tile.t): string =>
  Aba.mk(t.shards, t.children)
  |> Aba.join(of_delim(t), of_segment(~holes))
  |> String.concat("")
and of_delim = (t: Piece.tile, i: int): string => List.nth(t.label, i);

let to_string_basic = (z: Zipper.t): string => {
  z |> seg_of_zip |> of_segment(~holes=None);
};

let lines_to_list = String.split_on_char('\n');

let caret_str = "░";

let to_rows =
    (
      ~holes: option(string),
      ~measured: Measured.t,
      ~caret: option(Measured.Point.t),
      ~indent: string,
      ~segment: Segment.t,
    )
    : list(string) => {
  let indent_of = i => Measured.Rows.find(i, measured.rows).indent;
  let mk_indent = (i, r) => StringUtil.repeat(indent_of(i), indent) ++ r;
  let rows =
    segment |> of_segment(~holes) |> lines_to_list |> List.mapi(mk_indent);
  switch (caret) {
  | Some({row, col}) =>
    switch (ListUtil.split_nth_opt(row, rows)) {
    | Some((pre, caret_row, suf)) when col < String.length(caret_row) =>
      pre @ [StringUtil.insert_nth(col, caret_str, caret_row)] @ suf
    | Some((pre, caret_row, suf)) => pre @ [caret_row ++ caret_str] @ suf
    | _ => rows
    }
  | None => rows
  };
};

let pretty_print =
    (~holes: option(string)=Some(""), ~measured: Measured.t, z: Zipper.t)
    : string =>
  to_rows(
    ~holes,
    ~measured,
    ~caret=None,
    ~indent=" ",
    ~segment=seg_of_zip(z),
  )
  |> String.concat("\n");

let zipper_to_string =
    (~holes: option(string)=Some(""), z: Zipper.t): string =>
  to_rows(
    ~holes,
    ~measured=Zipper.measured(z),
    ~caret=None,
    ~indent="",
    ~segment=seg_of_zip(z),
  )
  |> String.concat("\n");

let to_string_editor =
    (~holes: option(string)=Some(""), editor: Editor.t): string =>
  zipper_to_string(~holes, editor.state.zipper);

let to_string_selection = (editor: Editor.t): string =>
  to_rows(
    ~measured=Zipper.measured(editor.state.zipper),
    ~caret=None,
    ~indent=" ",
    ~holes=None,
    ~segment=editor.state.zipper.selection.content,
  )
  |> String.concat("\n");

let zipper_of_string =
    (~zipper_init=Zipper.init(), str: string): option(Zipper.t) => {
  let insert = (z: option(Zipper.t), c: string): option(Zipper.t) => {
    let* z = z;
    try(c == "\r" ? Some(z) : Insert.go(c == "\n" ? Form.linebreak : c, z)) {
    | exn =>
      print_endline("WARN: zipper_of_string: " ++ Printexc.to_string(exn));
      None;
    };
  };
  str |> Util.StringUtil.to_list |> List.fold_left(insert, Some(zipper_init));
};

let paste_into_zip = (z: Zipper.t, str: string): option(Zipper.t) => {
  /* HACK(andrew): These two perform calls are a hack to
     deal with the fact that pasting something like "let a = b in"
     won't trigger the barfing of the "in"; to trigger this, we
     insert a space, and then we immediately delete it. */
  let settings = CoreSettings.off;
  let* z = zipper_of_string(~zipper_init=z, str);
  switch (Perform.go_z(~settings, Insert(" "), z)) {
  | Error(_) => None
  | Ok(z) =>
    switch (Perform.go_z(~settings, Destruct(Left), z)) {
    | Error(_) => None
    | Ok(z) => Some(z)
    }
  };
};
