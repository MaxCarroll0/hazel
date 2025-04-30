open Haz3lcore

type slice_info =
  | ExpCast of Exp.t * Id.t * Exp.t * TypSlice.t * TypSlice.t
  | ExpCastFail of Exp.t * Id.t * Exp.t * TypSlice.t * TypSlice.t
  | PatCast of Exp.t * Id.t * Pat.t * TypSlice.t * TypSlice.t

let cast_slice_info prog =
  let rec cast_slice_info (e : Exp.t) : slice_info list =
    match Exp.term_of e with
    | Cast (e', t1, t2) ->
        ExpCast (prog, Exp.rep_id e, e', t1, t2) :: cast_slice_info e'
    | FailedCast (e', t1, t2) ->
        ExpCastFail (prog, Exp.rep_id e, e', t1, t2) :: cast_slice_info e'
    | ListLit es | Tuple es | DeferredAp (_, es) ->
        List.flatten (List.map cast_slice_info es)
    | Ap (_, e1, e2)
    | Dot (e1, e2)
    | TupLabel (e1, e2)
    | Seq (e1, e2)
    | Cons (e1, e2)
    | ListConcat (e1, e2)
    | BinOp (_, e1, e2)
    | Let (_, e1, e2) ->
        cast_slice_info e1 @ cast_slice_info e2
    | If (e1, e2, e3) ->
        cast_slice_info e1 @ cast_slice_info e2 @ cast_slice_info e3
    | TypAp (e1, _) -> cast_slice_info e1
    | UnOp (_, e)
    | Test e
    | Filter (_, e)
    | Closure (_, e)
    | Parens e
    | Probe (e, _) ->
        cast_slice_info e
    | Match (e, branches) ->
        cast_slice_info e
        @ List.flatten
            (List.map (fun (p, b) -> pat_casts p @ cast_slice_info b) branches)
    | Fun (p, e, _, _) | FixF (p, e, _) -> pat_casts p @ cast_slice_info e
    | TypFun (_, body, _) -> cast_slice_info body
    | TyAlias (_, _, e) -> cast_slice_info e
    | Invalid _ | EmptyHole | MultiHole _
    | DynamicErrorHole (_, _)
    | Deferral _ | Undefined | Bool _ | Int _ | Float _ | String _
    | Constructor _ | Label _ | Var _ | BuiltinFun _ ->
        []
  and pat_casts (p : Pat.t) : slice_info list =
    match Pat.term_of p with
    | Cast (p', t1, t2) ->
        PatCast (prog, Pat.rep_id p, p', t1, t2) :: pat_casts p'
    | ListLit ps | Tuple ps -> List.flatten (List.map pat_casts ps)
    | Cons (p1, p2) | TupLabel (p1, p2) | Ap (p1, p2) ->
        pat_casts p1 @ pat_casts p2
    | Parens p | Probe (p, _) -> pat_casts p
    | Invalid _ | EmptyHole | MultiHole _ | Wild | Int _ | Float _ | Bool _
    | String _ | Constructor _ | Label _ | Var _ ->
        []
  in
  cast_slice_info prog

(* Hacky way to get the cast error size in most cases *)
let cast_error_size e =
  let size = ref 0 in
  let _ =
    Exp.map_term
      ~f_exp:(fun cont e ->
        match Exp.term_of e with
        | FailedCast (_, _, t) ->
            if !size = 0 then size := SlicingUtil.slice_size t;
            e
        | _ -> cont e)
      e
  in
  !size
