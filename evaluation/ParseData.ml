module B = Base
open Haz3lcore
module Fresh = IdTagged.FreshGrammar

let add_builtins e =
  (*Exp.map_term
    ~f_exp:(fun cont e ->
      match e.term with
      | Var x ->
          let builtin =
            VarMap.lookup Haz3lcore.Builtins.Pervasives.builtins x
          in
          cont
            (match builtin with
            | Some (Fn (_, _, _)) -> cont (Fresh.Exp.builtin_fun x)
            | Some (Const (_, _)) | None -> cont e)
      | _ -> cont e)
    *)
  e

(* Existing recovering parser *)
let make_term_parse s =
  add_builtins
    (MakeTerm.from_zip_for_sem (Option.get (Printer.zipper_of_string s))).term

let ill_typed =
  [
    [%blob "data/ill-typed/prog0001.hazel"];
    [%blob "data/ill-typed/prog0002.hazel"];
    [%blob "data/ill-typed/prog0003.hazel"];
    [%blob "data/ill-typed/prog0004.hazel"];
    [%blob "data/ill-typed/prog0005.hazel"];
  ]
  |> List.map make_term_parse

let well_typed =
  [ [%blob "data/well-typed/list.hazel"] ] 
