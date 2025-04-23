open Core_bench
open Haz3lcore
open ParseData
open SlicingUtil
open CastSliceUtil

let settings = CoreSettings.on (* Note: search off *)

(* Performance Benchmarks *)

(* Basic info relevant to slicing *)
type expression_info = {
  term : Exp.t;
  statics : Statics.Map.t;
  elaboration : Exp.t;
  result : DHExp.t; (* Deterministic eval result *)
}

let make_exp_info e =
  let statics = Statics.mk settings Builtins.ctx_init e in
  let elaboration, _ = Elaborator.elaborate statics e in
  let result, _ = Evaluator.evaluate ~env:Builtins.env_init e in
  { term = e; statics; elaboration; result }

let ill_typed = ill_typed |> List.map make_exp_info
let well_typed = well_typed |> List.map make_exp_info

(* Corpus Statistics *)

(* Effectiveness *)
(* Type Slice Size Stats*)
let slice_info l =
  l
  |> List.map (fun { term; statics; _ } -> slice_info statics term)
  |> List.flatten

let slice_info_all = ill_typed @ well_typed |> slice_info

(* Total slice size info
   Proportion of size of term + type. Type size approximates the checking context size *)
type slice_size = { term_type_size : int; slice_size : int; proportion : float }

let slice_sizes_all l =
  l
  |> List.map (fun (_, term, slice, _) ->
         (term_size term, term_size (TypSlice slice), slice_size slice))
  |> List.map (fun (term_size, type_size, slice_size) ->
         {
           term_type_size = term_size + type_size;
           slice_size;
           proportion =
             Float.of_int slice_size /. Float.of_int (term_size + type_size);
         })

let slice_sizes_ok l =
  l
  |> List.filter (function _, _, _, NoTypeError -> true | _ -> false)
  |> List.map (fun (_, term, slice, _) ->
         (term_size term, term_size (TypSlice slice), slice_size slice))
  |> List.filter (function _, _, 0 -> false | _ -> true)
     (* Filter empty slices, these are implicitly dynamic code or unsupported constructs*)
  |> List.map (fun (term_size, type_size, slice_size) ->
         {
           term_type_size = term_size + type_size;
           slice_size;
           proportion =
             Float.of_int slice_size /. Float.of_int (term_size + type_size);
         })

(* Slice size vs the combined slice size of the expectations*)
type slice_size_expectations_error = {
  slice_size : slice_size;
  expectations_slice_size : slice_size;
}

let slice_sizes_incon_expectations l =
  l
  |> List.filter_map (function
       | _, term, slice, Inconsistent { syn; ana } ->
           Some
             ( term_size term,
               term_size (TypSlice slice),
               slice_size slice,
               slice_size syn + slice_size ana )
       | _ -> None)
  |> List.filter (function _, _, 0, _ -> false | _ -> true)
     (* Filter empty slices, these are implicitly dynamic code or unsupported constructs*)
  |> List.map
       (fun (term_size, type_size, slice_size, expectations_slice_size) ->
         {
           slice_size =
             {
               term_type_size = term_size + type_size;
               slice_size;
               proportion =
                 Float.of_int slice_size /. Float.of_int (term_size + type_size);
             };
           expectations_slice_size =
             {
               term_type_size = term_size + type_size;
               slice_size = expectations_slice_size;
               proportion =
                 Float.of_int expectations_slice_size
                 /. Float.of_int (term_size + type_size);
             };
         })

(* Slice size vs the combined slice size of the branches*)
type slice_size_branches_error = {
  slice_size : slice_size;
  branches_slice_size : slice_size;
}

let slice_sizes_incon_branches l =
  l
  |> List.filter_map (function
       | _, term, slice, InconsistentBranches ss ->
           Some
             ( term_size term,
               term_size (TypSlice slice),
               slice_size slice,
               ss |> List.fold_left (fun acc s -> acc + slice_size s) 0 )
       | _ -> None)
  |> List.filter (function _, _, 0, _ -> false | _ -> true)
     (* Filter empty slices, these are implicitly dynamic code or unsupported constructs*)
  |> List.map (fun (term_size, type_size, slice_size, branches_slice_size) ->
         {
           slice_size =
             {
               term_type_size = term_size + type_size;
               slice_size;
               proportion =
                 Float.of_int slice_size /. Float.of_int (term_size + type_size);
             };
           branches_slice_size =
             {
               term_type_size = term_size + type_size;
               slice_size = branches_slice_size;
               proportion =
                 Float.of_int branches_slice_size
                 /. Float.of_int (term_size + type_size);
             };
         })

(* Simple average of term and slice size. Weighted average of proportions*)
type aggregate_slice_size = {
  avg_term_type_size : float;
  avg_slice_size : float;
  w_avg_proportion : float;
}

let aggregate_slice_sizes ss =
  List.fold_left
    (fun { avg_term_type_size; avg_slice_size; w_avg_proportion }
         { term_type_size; slice_size; proportion } ->
      {
        avg_term_type_size = avg_term_type_size +. Float.of_int term_type_size;
        avg_slice_size = avg_slice_size +. Float.of_int slice_size;
        w_avg_proportion =
          w_avg_proportion +. (Float.of_int term_type_size *. proportion);
      })
    { avg_term_type_size = 0.; avg_slice_size = 0.; w_avg_proportion = 0. }
    ss
  |> fun { avg_term_type_size; avg_slice_size; w_avg_proportion } ->
  {
    avg_term_type_size =
      avg_term_type_size *. 1. /. Float.of_int (List.length ss);
    avg_slice_size = avg_slice_size *. 1. /. Float.of_int (List.length ss);
    w_avg_proportion =
      w_avg_proportion *. 1.
      /. Float.of_int
           (List.fold_left (fun acc s -> acc + s.term_type_size) 0 ss);
  }

(* Type Slicing Sizes *)
let cast_slice_info_elaborated l =
  l
  |> List.map (fun { elaboration; _ } -> cast_slice_info elaboration)
  |> List.flatten

let cast_slice_info_results l =
  l
  |> List.map (fun { elaboration; _ } -> cast_slice_info elaboration)
  |> List.flatten

let cast_slice_info_all =
  ill_typed @ well_typed |> fun l ->
  cast_slice_info_elaborated l @ cast_slice_info_results l

(* Slice size info
   Ratio between slice casted from size and the casted term size
   Ratio between slice casted to size and it's type *)
type cast_slice_size = {
  term_size : int;
  type_size : int;
  slice_from_size : int;
  slice_to_size : int;
  ratio_from_term : float;
  ratio_to_type : float;
}

let cast_slice_sizes_all l =
  l
  |> List.map (function
       | ExpCast (_, e, t1, t2) | ExpCastFail (_, e, t1, t2) ->
           {
             term_size = term_size (Exp e);
             type_size = term_size (TypSlice t2);
             slice_from_size = slice_size t1;
             slice_to_size = slice_size t2;
             ratio_from_term =
               Float.of_int (slice_size t1) /. Float.of_int (term_size (Exp e));
             ratio_to_type =
               Float.of_int (slice_size t2)
               /. Float.of_int (term_size (TypSlice t2));
           }
       | PatCast (_, p, t1, t2) ->
           {
             term_size = term_size (Pat p);
             type_size = term_size (TypSlice t2);
             slice_from_size = slice_size t1;
             slice_to_size = slice_size t2;
             ratio_from_term =
               Float.of_int (slice_size t1) /. Float.of_int (term_size (Pat p));
             ratio_to_type =
               Float.of_int (slice_size t2)
               /. Float.of_int (term_size (TypSlice t2));
           })

let cast_slice_sizes_ok l =
  l
  |> List.filter_map (function
       | ExpCastFail _ -> None
       | ExpCast (_, e, t1, t2) ->
           Some
             {
               term_size = term_size (Exp e);
               type_size = term_size (TypSlice t2);
               slice_from_size = slice_size t1;
               slice_to_size = slice_size t2;
               ratio_from_term =
                 Float.of_int (slice_size t1)
                 /. Float.of_int (term_size (Exp e));
               ratio_to_type =
                 Float.of_int (slice_size t2)
                 /. Float.of_int (term_size (TypSlice t2));
             }
       | PatCast (_, p, t1, t2) ->
           Some
             {
               term_size = term_size (Pat p);
               type_size = term_size (TypSlice t2);
               slice_from_size = slice_size t1;
               slice_to_size = slice_size t2;
               ratio_from_term =
                 Float.of_int (slice_size t1)
                 /. Float.of_int (term_size (Pat p));
               ratio_to_type =
                 Float.of_int (slice_size t2)
                 /. Float.of_int (term_size (TypSlice t2));
             })

let cast_slice_sizes_pats l =
  l
  |> List.filter_map (function
       | ExpCastFail _ | ExpCast _ -> None
       | PatCast (_, p, t1, t2) ->
           Some
             {
               term_size = term_size (Pat p);
               type_size = term_size (TypSlice t2);
               slice_from_size = slice_size t1;
               slice_to_size = slice_size t2;
               ratio_from_term =
                 Float.of_int (slice_size t1)
                 /. Float.of_int (term_size (Pat p));
               ratio_to_type =
                 Float.of_int (slice_size t2)
                 /. Float.of_int (term_size (TypSlice t2));
             })

let cast_slice_sizes_errors l =
  l
  |> List.filter_map (function
       | ExpCastFail (_, e, t1, t2) ->
           Some
             {
               term_size = term_size (Exp e);
               type_size = term_size (TypSlice t2);
               slice_from_size = slice_size t1;
               slice_to_size = slice_size t2;
               ratio_from_term =
                 Float.of_int (slice_size t1)
                 /. Float.of_int (term_size (Exp e));
               ratio_to_type =
                 Float.of_int (slice_size t2)
                 /. Float.of_int (term_size (TypSlice t2));
             }
       | ExpCast _ | PatCast _ -> None)

type aggregate_cast_slice_size = {
  avg_term_size : float;
  avg_type_size : float;
  avg_slice_from_size : float;
  avg_slice_to_size : float;
  w_avg_ratio_from_term : float;
  w_avg_ratio_to_type : float;
}

let aggregate_cast_slice_sizes ss =
  List.fold_left
    (fun {
           avg_term_size;
           avg_type_size;
           avg_slice_from_size;
           avg_slice_to_size;
           w_avg_ratio_from_term;
           w_avg_ratio_to_type;
         }
         {
           term_size;
           type_size;
           slice_from_size;
           slice_to_size;
           ratio_from_term;
           ratio_to_type;
         } ->
      {
        avg_term_size = avg_term_size +. Float.of_int term_size;
        avg_type_size = avg_type_size +. Float.of_int type_size;
        avg_slice_from_size =
          avg_slice_from_size +. Float.of_int slice_from_size;
        avg_slice_to_size = avg_slice_to_size +. Float.of_int slice_to_size;
        w_avg_ratio_from_term =
          w_avg_ratio_from_term +. (Float.of_int term_size *. ratio_from_term);
        w_avg_ratio_to_type =
          w_avg_ratio_to_type +. (Float.of_int type_size *. ratio_to_type);
      })
    {
      avg_term_size = 0.;
      avg_type_size = 0.;
      avg_slice_from_size = 0.;
      avg_slice_to_size = 0.;
      w_avg_ratio_from_term = 0.;
      w_avg_ratio_to_type = 0.;
    }
    ss
  |>
  fun {
        avg_term_size;
        avg_type_size;
        avg_slice_from_size;
        avg_slice_to_size;
        w_avg_ratio_from_term;
        w_avg_ratio_to_type;
      }
  ->
  {
    (* Exclude empty slices from aggregates: these are generally to/fro the dynamic type or unsupported errors/constructs*)
    avg_term_size =
      avg_term_size *. 1.
      /. Float.of_int
           (ss |> List.filter (fun s -> s.slice_from_size != 0) |> List.length);
    avg_type_size =
      avg_type_size *. 1.
      /. Float.of_int
           (ss |> List.filter (fun s -> s.slice_to_size != 0) |> List.length);
    avg_slice_from_size =
      avg_slice_from_size *. 1.
      /. Float.of_int
           (ss |> List.filter (fun s -> s.slice_from_size != 0) |> List.length);
    avg_slice_to_size =
      avg_slice_to_size *. 1.
      /. Float.of_int
           (ss |> List.filter (fun s -> s.slice_to_size != 0) |> List.length);
    w_avg_ratio_from_term =
      w_avg_ratio_from_term *. 1.
      /. Float.of_int
           (List.fold_left
              (fun acc s ->
                acc + if s.slice_from_size != 0 then s.term_size else 0)
              0 ss);
    w_avg_ratio_to_type =
      w_avg_ratio_to_type *. 1.
      /. Float.of_int
           (List.fold_left
              (fun acc s ->
                acc + if s.slice_to_size != 0 then s.type_size else 0)
              0 ss);
  }
