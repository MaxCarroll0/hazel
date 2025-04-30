module B = Base
open Haz3lcore
module Fresh = IdTagged.FreshGrammar

(* The current version of Hazel does not correctly parse any variable starting with "in" when inside a let expression *)
(* I delete any in[a-Z,0-9] to temporarily get around this *)
(* This must also be performed on the Ctx, see Settings.ctx *)
let replace_inC =
  Re.replace ~all:true (Re.Perl.compile_pat " in([a-zA-Z0-9])") ~f:(fun g ->
      Re.Group.get g 1)

let add_builtins e =
  Exp.map_term
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
    e

let rec ap_of_typ t e =
  match Typ.term_of t with
  | Forall (_, t2) ->
      Exp.fresh (TypAp (e, Typ.hole [] |> Typ.fresh)) |> ap_of_typ t2
  | Arrow (_, t2) ->
      Exp.fresh (Ap (Forward, e, Exp.fresh EmptyHole)) |> ap_of_typ t2
  | _ -> e

(* Note: Adds search points at EVERY hole *)
let rec add_search_points (statics : Statics.Map.t) e =
  Exp.map_term
    ~f_exp:(fun cont e ->
      match e.term with
      | EmptyHole -> (
          match Statics.Map.lookup (Exp.rep_id e) statics with
          | None -> failwith "No statics map for hole"
          | Some info ->
              Ctx.added_bindings (Info.ctx_of info) Settings.ctx
              |> List.filter_map (function
                   | Ctx.VarEntry { name; typ; _ } ->
                       Some
                         (ap_of_typ (TypSlice.typ_of typ) (Var name |> Exp.fresh))
                   | _ -> None)
              |> fun es -> Tuple es |> Exp.fresh)
      | _ -> cont e)
    e

(* Existing recovering parser *)
let make_term_parse s =
  s |> replace_inC |> fun s ->
  ( add_builtins
      (MakeTerm.from_zip_for_sem (Option.get (Printer.zipper_of_string s))).term
  |> fun x ->
    print_endline ("Successfully parsed:\n" ^ s);
    x )
  |> fun e ->
  (Statics.mk Settings.settings Settings.ctx e, e) |> fun x ->
  print_endline "Successfully Type Checked";
  x |> fun (statics, e) ->
  add_search_points statics e |> fun e ->
  print_endline "Inserted search points";
  e

let well_typed = []
(* Data.well_typed |> List.map (fun s -> try Some (make_term_parse s) with _ -> None) *)

let ill_typed_annotated =
  Data.ill_typed_annotated |> fun x ->
  (print_endline "Started parsing ill typed annotated";
   x)
  |> List.filter_map (fun s -> try Some (make_term_parse s) with _ -> None)
  |> fun x ->
  print_endline "Finished parsing ill typed dynamic";
  x

let ill_typed_dynamic =
  Data.ill_typed_dynamic |> fun x ->
  (print_endline "Started parsing ill typed dynamic";
   x)
  |> List.filter_map (fun s -> try Some (make_term_parse s) with _ -> None)
  |> fun x ->
  print_endline "Finished parsing ill typed dynamic";
  x
