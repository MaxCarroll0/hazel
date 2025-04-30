open Haz3lcore

let term_size (e : 'a Grammar.any_t) =
  let id_count = ref 0 in
  let incr_count (type a) cont (e : a IdTagged.t) =
    id_count := !id_count + List.length e.annotation.ids;
    cont e
  in
  let _ =
    e
    |> Any.map_term ~f_exp:incr_count ~f_typ:incr_count ~f_pat:incr_count
         ~f_typslice:incr_count ~f_rul:incr_count
  in
  !id_count

let rec remove y = function
  | [] -> []
  | x :: xs when Id.equal x y -> xs
  | x :: xs -> x :: xs

let rec remove_duplicates = function
  | [] -> []
  | x :: xs -> x :: remove_duplicates (remove x xs)

let term_ids (e : 'a Grammar.any_t) =
  let ids = ref [] in
  let incr_count (type a) cont e =
    ids := IdTagged.ids e @ !ids;
    cont e
  in
  let _ =
    e
    |> Any.map_term ~f_exp:incr_count ~f_typ:incr_count ~f_pat:incr_count
         ~f_typslice:incr_count ~f_rul:incr_count
  in
  !ids

let rec diff xs = function [] -> xs | y :: ys -> diff (remove y xs) ys

let slice_size (s : TypSlice.t) =
  (TypSlice.full_slice s.term).term_ids |> remove_duplicates |> List.length

type error_slice_info =
  | NoTypeError
  | BadTrivAp of TypSlice.t (* Analysis slice of the arrow type *)
  | Inconsistent of {
      syn : TypSlice.t;
      ana : TypSlice.t;
      incon_join : (TypSlice.t * TypSlice.t) list;
    }
  | InconsistentBranches of TypSlice.t list * (TypSlice.t * TypSlice.t) list
(* Branch synthesis slices *)
(* Expected constructor is also an error with slice, but the slice size is always exactly 1, so pointless analysing *)

type slice_info =
  Any.t * Id.t * IdTagged.IdTag.t Grammar.any_t * TypSlice.t * error_slice_info

let common_error_slice_info : Info.error_common -> error_slice_info = function
  | NoType
      ( BadToken _ | FreeConstructor _ | WantTuple
      | LabelNotFound (_, _)
      | BadLabel _ | InvalidLabel _ )
  | DuplicateLabel _ | TupleLabelError _ ->
      NoTypeError
  | NoType (BadTrivAp ana) -> BadTrivAp ana
  | Inconsistent (Expectation { syn; ana; incon_join }) ->
      Inconsistent { syn; ana; incon_join }
  | Inconsistent (Internal (branch_tys, incon_join)) ->
      InconsistentBranches (branch_tys, incon_join)
  (* Arrows can be considered as analysing against ? -> ? with slice being the aplication ?(?) *)
  | Inconsistent (WithArrow (arrow, ana, incon_join)) ->
      Inconsistent { syn = arrow; ana; incon_join }

let slice_info statics e : slice_info list =
  statics |> Id.Map.to_list
  |> List.map (fun (id, info) ->
         match info with
         | Info.InfoExp exp ->
             Some
               ( e,
                 id,
                 Grammar.Exp exp.term,
                 exp.ty,
                 match exp.status with
                 | NotInHole _
                 | InHole
                     ( FreeVariable _ | InexhaustiveMatch _ | UnusedDeferral
                     | BadPartialAp _ ) ->
                     NoTypeError
                 | InHole (Common err) -> common_error_slice_info err )
         | Info.InfoPat pat ->
             Some
               ( e,
                 id,
                 Pat pat.term,
                 pat.ty,
                 match pat.status with
                 | NotInHole _ | InHole (ExpectedConstructor _ | Redundant _) ->
                     NoTypeError
                 | InHole (Common err) -> common_error_slice_info err )
         | Info.InfoTyp _ | Info.InfoTPat _ | Info.Secondary _ -> None)
  |> List.filter Option.is_some |> List.map Option.get
