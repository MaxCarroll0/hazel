open Util
open ParseData
open SlicingUtil
open CastSliceUtil
open Core_bench
open Haz3lcore

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
     (* TODO: Use simplified inconsistency slice joins as the error slice here *)
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
  error_slice_size : slice_size;
  combined_slice_size : slice_size;
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
  (* TODO: Use simplified inconsistency slice joins as the error slice here *)
  |> List.map
       (fun (term_size, type_size, slice_size, expectations_slice_size) ->
         {
           error_slice_size =
             (* TODO, error slicing *)
             {
               term_type_size = term_size + type_size;
               slice_size;
               proportion =
                 Float.of_int slice_size /. Float.of_int (term_size + type_size);
             };
           combined_slice_size =
             {
               term_type_size = term_size + type_size;
               slice_size = expectations_slice_size;
               proportion =
                 Float.of_int expectations_slice_size
                 /. Float.of_int (term_size + type_size);
             };
         })

(* Slice size vs the combined slice size of the branches*)
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
  (* TODO: Use simplified inconsistency slice joins as the error slice here *)
  |> List.map (fun (term_size, type_size, slice_size, branches_slice_size) ->
         {
           error_slice_size =
             (* TODO, error slicing *)
             {
               term_type_size = term_size + type_size;
               slice_size;
               proportion =
                 Float.of_int slice_size /. Float.of_int (term_size + type_size);
             };
           combined_slice_size =
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
  std_term_type_size : float;
  avg_slice_size : float;
  std_slice_size : float;
  w_avg_proportion : float;
  w_std_proportion : float;
}

let aggregate_slice_sizes ss =
  {
    avg_term_type_size =
      avg (List.map (fun s -> s.term_type_size) ss |> to_floats);
    std_term_type_size =
      std (List.map (fun s -> s.term_type_size) ss |> to_floats);
    avg_slice_size = avg_0 (List.map (fun s -> s.slice_size) ss |> to_floats);
    std_slice_size = std_0 (List.map (fun s -> s.slice_size) ss |> to_floats);
    w_avg_proportion =
      w_avg_0
        (List.map (fun s -> s.proportion) ss)
        (List.map (fun s -> s.term_type_size) ss |> to_floats);
    w_std_proportion =
      w_std_0
        (List.map (fun s -> s.proportion) ss)
        (List.map (fun s -> s.term_type_size) ss |> to_floats);
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
  std_term_size : float;
  avg_type_size : float;
  std_type_size : float;
  avg_slice_from_size : float;
  std_slice_from_size : float;
  avg_slice_to_size : float;
  std_slice_to_size : float;
  w_avg_ratio_from_term : float;
  w_std_ratio_from_term : float;
  w_avg_ratio_to_type : float;
  w_std_ratio_to_type : float;
}

let aggregate_cast_slice_sizes ss =
  {
    avg_term_size = avg (List.map (fun s -> s.term_size) ss |> to_floats);
    std_term_size = std (List.map (fun s -> s.term_size) ss |> to_floats);
    avg_type_size = avg (List.map (fun s -> s.type_size) ss |> to_floats);
    std_type_size = std (List.map (fun s -> s.type_size) ss |> to_floats);
    avg_slice_from_size =
      avg_0 (List.map (fun s -> s.slice_from_size) ss |> to_floats);
    std_slice_from_size =
      std_0 (List.map (fun s -> s.slice_from_size) ss |> to_floats);
    avg_slice_to_size =
      avg_0 (List.map (fun s -> s.slice_to_size) ss |> to_floats);
    std_slice_to_size =
      std_0 (List.map (fun s -> s.slice_to_size) ss |> to_floats);
    w_avg_ratio_from_term =
      w_avg_0
        (List.map (fun s -> s.ratio_from_term) ss)
        (List.map (fun s -> s.term_size) ss |> to_floats);
    w_std_ratio_from_term =
      w_avg_0
        (List.map (fun s -> s.ratio_from_term) ss)
        (List.map (fun s -> s.term_size) ss |> to_floats);
    w_avg_ratio_to_type =
      w_avg_0
        (List.map (fun s -> s.ratio_to_type) ss)
        (List.map (fun s -> s.type_size) ss |> to_floats);
    w_std_ratio_to_type =
      w_avg_0
        (List.map (fun s -> s.ratio_to_type) ss)
        (List.map (fun s -> s.type_size) ss |> to_floats);
  }

(* Search Procedure Proportions *)
(* Limiting the running limit to ~60s and 1GB using garbage collector alarms *)
open ResourceLimits
module DFS = Nondeterminism.DFS
module SearchDFS = IndetEvaluator.Make (DFS)
module IDFS = Nondeterminism.IDFS
module SearchIDFS = IndetEvaluator.Make (IDFS)
module BFS = Nondeterminism.BFS
module SearchBFS = IndetEvaluator.Make (BFS)

(* Bounded depth increments of 5 *)
module BDFS =
  Nondeterminism.Bounded ((val Nondeterminism.const_incr_config ~init:5 ~inc:5))

module SearchBDFS = IndetEvaluator.Make (BDFS)

let dfs d =
  run_with_limits (fun () ->
      DFS.once (SearchDFS.cast_errors ~env:Builtins.env_init d))

let bfs d =
  run_with_limits (fun () ->
      BFS.once (SearchBFS.cast_errors ~env:Builtins.env_init d))

let idfs d =
  run_with_limits (fun () ->
      IDFS.once (SearchIDFS.cast_errors ~env:Builtins.env_init d))

let bdfs d =
  run_with_limits (fun () ->
      BDFS.once (SearchBDFS.cast_errors ~env:Builtins.env_init d))

(* Cast size is of the type casted TO, not much reason to inspect the cast from given we have a concrete value to explain it *)
(* TODO: cast depedence*)
(* TODO: Code coverage when time outs occur *)
type search_result =
  | Witness of {
      trace_size : int;
      witness_size : int;
          (* Sum of sizes of ALL instantiated parts, even if the instantiation is not actually the erroneous part of the witness *)
      code_coverage : float;
      cast_size : int;
      result : Exp.t;
    }
  | NoWitness
  | TimeOut
  | MemoryExceeded

let eval_results search l =
  l
  |> List.map (fun s ->
         try
           match search s.elaboration with
           | None -> NoWitness
           | Some (state, result) ->
               Witness
                 {
                   trace_size = IndetEvaluatorState.get_trace_length state;
                   witness_size = IndetEvaluatorState.get_instantiations state;
                   code_coverage =
                     Float.of_int
                       (List.length
                          (diff
                             (term_ids (Exp s.elaboration))
                             [] (* IndetEvaluatorState.get_ids_covered *)))
                     /. Float.of_int
                          (List.length (term_ids (Exp s.elaboration)));
                   cast_size =
                     (function
                      | { term = FailedCast (_, _, t); _ } -> slice_size t
                      | _ -> 0
                       : Exp.t -> int)
                       result;
                   result;
                 }
         with
         | ExceededTimeLimit _ -> TimeOut
         | ExceededMemoryLimit _ -> MemoryExceeded)

let dfs_results = eval_results dfs
let bfs_results = eval_results bfs
let dfs_results = eval_results dfs
let idfs_results = eval_results idfs
let bdfs_results = eval_results bdfs
