type t = {
  active: bool,
  valid_text: bool,
  current_text: string,
  error_text: string,
};

[@deriving sexp]
type update =
  | OpenEditor(Program.t)
  | CloseEditor
  | ToggleValid
  | ClearError
  | SetCurrentText(string)
  | SetErrorText(string);

let init: t;

let apply_update: (update, t) => t;

/*
 * Returns the count of newlines in current_text
 */
let line_count: t => int;
