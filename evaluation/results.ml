open Util
open ParseData
open SlicingUtil
open CastSliceUtil
open Core_bench
open Haz3lcore

(* Performance Benchmarks *)

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

(* Basic info relevant to slicing *)
type expression_info = {
  term : Exp.t;
  statics : Statics.Map.t;
  elaboration : Exp.t;
  result : DHExp.t; (* Deterministic eval result *)
  trace_length : int; (* Deterministic trace length *)
}

let make_exp_info e =
  let statics = Statics.mk Settings.settings Settings.ctx e in
  let elaboration, _ = Elaborator.elaborate statics e in
  let state, result =
    DFS.once
      (SearchDFS.deterministic ~env:Builtins.env_init
         ~state:IndetEvaluatorState.init e)
    |> Option.get
  in
  {
    term = e;
    statics;
    elaboration;
    result;
    trace_length = IndetEvaluatorState.get_trace_length state;
  }

let ill_typed_annotated =
  ill_typed_annotated
  |> List.filter_map (fun e -> try Some (make_exp_info e) with _ -> None)
  |> List.filteri (fun i _ -> i < 25)

let ill_typed_dynamic =
  ill_typed_dynamic
  |> List.filter_map (fun e -> try Some (make_exp_info e) with _ -> None)

let ill_typed = ill_typed_annotated @ ill_typed_dynamic

let well_typed =
  well_typed
  |> List.filter_map (fun e -> try Some (make_exp_info e) with _ -> None)

let all = well_typed @ ill_typed

(* Corpus Statistics *)
type corpus_stats = {
  num_progs : int;
  avg_prog_size : float;
  std_prog_size : float;
  avg_trace_size : float;
  std_trace_size : float;
}

let aggregate_corpus_stats l =
  {
    num_progs = List.length l;
    avg_prog_size =
      avg_0 (List.map (fun i -> Float.of_int (term_size (Exp i.term))) l);
    std_prog_size =
      std_0 (List.map (fun i -> Float.of_int (term_size (Exp i.term))) l);
    avg_trace_size = avg (List.map (fun i -> Float.of_int i.trace_length) l);
    std_trace_size = std (List.map (fun i -> Float.of_int i.trace_length) l);
  }

(* Effectiveness *)
(* Type Slice Size Stats*)
let slice_info l =
  l
  |> List.map (fun { term; statics; _ } -> slice_info statics (Exp term))
  |> List.flatten

let slice_info_all = all |> slice_info

(* Total slice size info
   Proportion of size of term + type. Type size approximates the checking context size *)
type slice_size = {
  prog_size : int;
  type_size : int;
  slice_size : int;
  proportion_prog : float;
  ratio_typ : float;
}

let slice_sizes_all l =
  l
  |> List.map (fun (prog, _, _, slice, _) ->
         (term_size prog, term_size (TypSlice slice), slice_size slice))
     (* TODO: Use simplified inconsistency slice joins as the error slice here *)
  |> List.map (fun (prog_size, type_size, slice_size) ->
         {
           prog_size;
           type_size;
           slice_size;
           proportion_prog = Float.of_int slice_size /. Float.of_int prog_size;
           ratio_typ = Float.of_int slice_size /. Float.of_int type_size;
         })

let slice_sizes_ok l =
  l
  |> List.filter (function _, _, _, _, NoTypeError -> true | _ -> false)
  |> List.map (fun (prog, _, _, slice, _) ->
         (term_size prog, term_size (TypSlice slice), slice_size slice))
  |> List.filter (function _, _, 0 -> false | _ -> true)
     (* Filter empty slices, these are implicitly dynamic code or unsupported constructs*)
  |> List.map (fun (prog_size, type_size, slice_size) ->
         {
           prog_size;
           type_size;
           slice_size;
           proportion_prog = Float.of_int slice_size /. Float.of_int prog_size;
           ratio_typ = Float.of_int slice_size /. Float.of_int type_size;
         })

(* Slice size vs the combined slice size of the expectations*)
type slice_size_expectations_error = {
  error_slice_size : slice_size;
  combined_slice_size : slice_size;
}

let slice_sizes_incon_expectations l =
  l
  |> List.filter_map (function
       | prog, _, _, slice, Inconsistent { syn; ana; incon_join } ->
           Some
             ( term_size prog,
               term_size (TypSlice slice),
               slice_size slice,
               slice_size syn + slice_size ana,
               (incon_join
               |> List.map (fun (x, y) ->
                      TypSlice.union_slice_incr
                        (TypSlice.full_slice (TypSlice.term_of x))
                        (TypSlice.full_slice (TypSlice.term_of y)))
               |> List.fold_left TypSlice.union_slice_incr
                    TypSlice.empty_slice_incr)
                 .term_ids |> List.length )
       | _ -> None)
  |> List.filter (function _, _, _, _, 0 -> false | _ -> true)
  (* Filter empty slices, these are implicitly dynamic code or unsupported constructs*)
  (* TODO: Use simplified inconsistency slice joins as the error slice here *)
  |> List.map
       (fun
         ( prog_size,
           type_size,
           slice_size,
           expectations_slice_size,
           error_slice_size )
       ->
         {
           error_slice_size =
             {
               prog_size;
               slice_size = error_slice_size;
               type_size;
               proportion_prog =
                 Float.of_int error_slice_size /. Float.of_int prog_size;
               ratio_typ =
                 Float.of_int error_slice_size /. Float.of_int prog_size;
             };
           combined_slice_size =
             {
               prog_size;
               type_size;
               slice_size = slice_size + expectations_slice_size;
               proportion_prog =
                 Float.of_int (slice_size + expectations_slice_size)
                 /. Float.of_int prog_size;
               ratio_typ =
                 Float.of_int (slice_size + expectations_slice_size)
                 /. Float.of_int type_size;
             };
         })

(* Slice size vs the combined slice size of the branches*)
let slice_sizes_incon_branches l =
  l
  |> List.filter_map (function
       | prog, _, _, slice, InconsistentBranches (ss, incon_join) ->
           Some
             ( term_size prog,
               term_size (TypSlice slice),
               slice_size slice,
               ss |> List.fold_left (fun acc s -> acc + slice_size s) 0,
               (incon_join
               |> List.map (fun (x, y) ->
                      TypSlice.union_slice_incr
                        (TypSlice.full_slice (TypSlice.term_of x))
                        (TypSlice.full_slice (TypSlice.term_of y)))
               |> List.fold_left TypSlice.union_slice_incr
                    TypSlice.empty_slice_incr)
                 .term_ids |> List.length )
       | _ -> None)
  |> List.filter (function _, _, _, _, 0 -> false | _ -> true)
  (* Filter empty slices, these are implicitly dynamic code or unsupported constructs*)
  |> List.map
       (fun
         ( prog_size,
           type_size,
           slice_size,
           branches_slice_size,
           error_slice_size )
       ->
         {
           error_slice_size =
             {
               prog_size;
               slice_size = error_slice_size;
               type_size;
               proportion_prog =
                 Float.of_int error_slice_size /. Float.of_int prog_size;
               ratio_typ =
                 Float.of_int error_slice_size /. Float.of_int type_size;
             };
           combined_slice_size =
             {
               prog_size;
               slice_size = slice_size + branches_slice_size;
               type_size;
               proportion_prog =
                 Float.of_int (slice_size + branches_slice_size)
                 /. Float.of_int prog_size;
               ratio_typ =
                 Float.of_int (slice_size + branches_slice_size)
                 /. Float.of_int type_size;
             };
         })

(* Simple average of term and slice size. Weighted average of proportions *)
(* Avg prog size here is weighted by the number of slices considered in the prog*)
type aggregate_slice_size = {
  avg_prog_size : float;
  std_prog_size : float;
  avg_slice_size : float;
  std_slice_size : float;
  w_avg_proportion_prog : float;
  w_std_proportion_prog : float;
  w_avg_ratio_typ : float;
  w_std_ratio_typ : float;
}

let aggregate_slice_sizes ss =
  {
    avg_prog_size = avg (List.map (fun s -> s.prog_size) ss |> to_floats);
    std_prog_size = std (List.map (fun s -> s.prog_size) ss |> to_floats);
    avg_slice_size = avg_0 (List.map (fun s -> s.slice_size) ss |> to_floats);
    std_slice_size = std_0 (List.map (fun s -> s.slice_size) ss |> to_floats);
    w_avg_proportion_prog =
      w_avg_0
        (List.map (fun s -> s.proportion_prog) ss)
        (List.map (fun s -> s.prog_size) ss |> to_floats);
    w_std_proportion_prog =
      w_std_0
        (List.map (fun s -> s.proportion_prog) ss)
        (List.map (fun s -> s.prog_size) ss |> to_floats);
    w_avg_ratio_typ =
      w_avg_0
        (List.map (fun s -> s.ratio_typ) ss)
        (List.map (fun s -> s.type_size) ss |> to_floats);
    w_std_ratio_typ =
      w_std_0
        (List.map (fun s -> s.ratio_typ) ss)
        (List.map (fun s -> s.type_size) ss |> to_floats);
  }

let aggregate_error_slice_sizes ss =
  ( ss |> List.map (fun s -> s.error_slice_size) |> aggregate_slice_sizes,
    ss |> List.map (fun s -> s.combined_slice_size) |> aggregate_slice_sizes )

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
  prog_size : int;
  term_size : int;
  type_size : int;
  slice_from_size : int;
  slice_to_size : int;
  proportion_prog : float;
  ratio_typ : float;
}

let cast_slice_sizes_all l =
  l
  |> List.map (function
       | ExpCast (prog, _, e, t1, t2) | ExpCastFail (prog, _, e, t1, t2) ->
           {
             prog_size = term_size (Exp prog);
             term_size = term_size (Exp e);
             type_size = term_size (TypSlice t2);
             slice_from_size = slice_size t1;
             slice_to_size = slice_size t2;
             proportion_prog =
               Float.of_int (slice_size t1)
               /. Float.of_int (term_size (Exp prog));
             ratio_typ =
               Float.of_int (slice_size t2)
               /. Float.of_int (term_size (TypSlice t2));
           }
       | PatCast (prog, _, p, t1, t2) ->
           {
             prog_size = term_size (Exp prog);
             term_size = term_size (Pat p);
             type_size = term_size (TypSlice t2);
             slice_from_size = slice_size t1;
             slice_to_size = slice_size t2;
             proportion_prog =
               Float.of_int (slice_size t1)
               /. Float.of_int (term_size (Exp prog));
             ratio_typ =
               Float.of_int (slice_size t2)
               /. Float.of_int (term_size (TypSlice t2));
           })

let cast_slice_sizes_ok l =
  l
  |> List.filter_map (function
       | ExpCastFail _ -> None
       | ExpCast (prog, _, e, t1, t2) ->
           Some
             {
               prog_size = term_size (Exp prog);
               term_size = term_size (Exp e);
               type_size = term_size (TypSlice t2);
               slice_from_size = slice_size t1;
               slice_to_size = slice_size t2;
               proportion_prog =
                 Float.of_int (slice_size t1)
                 /. Float.of_int (term_size (Exp prog));
               ratio_typ =
                 Float.of_int (slice_size t2)
                 /. Float.of_int (term_size (TypSlice t2));
             }
       | PatCast (prog, _, p, t1, t2) ->
           Some
             {
               prog_size = term_size (Exp prog);
               term_size = term_size (Pat p);
               type_size = term_size (TypSlice t2);
               slice_from_size = slice_size t1;
               slice_to_size = slice_size t2;
               proportion_prog =
                 Float.of_int (slice_size t1)
                 /. Float.of_int (term_size (Exp prog));
               ratio_typ =
                 Float.of_int (slice_size t2)
                 /. Float.of_int (term_size (TypSlice t2));
             })

let cast_slice_sizes_pats l =
  l
  |> List.filter_map (function
       | ExpCastFail _ | ExpCast _ -> None
       | PatCast (prog, _, p, t1, t2) ->
           Some
             {
               prog_size = term_size (Exp prog);
               term_size = term_size (Pat p);
               type_size = term_size (TypSlice t2);
               slice_from_size = slice_size t1;
               slice_to_size = slice_size t2;
               proportion_prog =
                 Float.of_int (slice_size t1)
                 /. Float.of_int (term_size (Exp prog));
               ratio_typ =
                 Float.of_int (slice_size t2)
                 /. Float.of_int (term_size (TypSlice t2));
             })

let cast_slice_sizes_errors l =
  l
  |> List.filter_map (function
       | ExpCastFail (prog, _, e, t1, t2) ->
           Some
             {
               prog_size = term_size (Exp prog);
               term_size = term_size (Exp e);
               type_size = term_size (TypSlice t2);
               slice_from_size = slice_size t1;
               slice_to_size = slice_size t2;
               proportion_prog =
                 Float.of_int (slice_size t1)
                 /. Float.of_int (term_size (Exp prog));
               ratio_typ =
                 Float.of_int (slice_size t2)
                 /. Float.of_int (term_size (TypSlice t2));
             }
       | ExpCast _ | PatCast _ -> None)

type aggregate_cast_slice_size = {
  avg_prog_size : float;
  std_prog_size : float;
  avg_term_size : float;
  std_term_size : float;
  avg_type_size : float;
  std_type_size : float;
  avg_slice_from_size : float;
  std_slice_from_size : float;
  avg_slice_to_size : float;
  std_slice_to_size : float;
  w_avg_proportion_prog : float;
  w_std_proportion_prog : float;
  w_avg_ratio_typ : float;
  w_std_ratio_typ : float;
}

let aggregate_cast_slice_sizes ss =
  {
    avg_prog_size = avg (List.map (fun s -> s.prog_size) ss |> to_floats);
    std_prog_size = std (List.map (fun s -> s.prog_size) ss |> to_floats);
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
    w_avg_proportion_prog =
      w_avg_0
        (List.map (fun s -> s.proportion_prog) ss)
        (List.map (fun s -> s.prog_size) ss |> to_floats);
    w_std_proportion_prog =
      w_avg_0
        (List.map (fun s -> s.proportion_prog) ss)
        (List.map (fun s -> s.prog_size) ss |> to_floats);
    w_avg_ratio_typ =
      w_avg_0
        (List.map (fun s -> s.ratio_typ) ss)
        (List.map (fun s -> s.type_size) ss |> to_floats);
    w_std_ratio_typ =
      w_avg_0
        (List.map (fun s -> s.ratio_typ) ss)
        (List.map (fun s -> s.type_size) ss |> to_floats);
  }

(* Search Procedure Proportions *)

exception Timeout

let with_timeout ~secs f =
  let _ =
    Sys.set_signal Sys.sigalrm (Sys.Signal_handle (fun _ -> raise Timeout))
  in
  ignore (Unix.alarm secs);
  try
    let r = f () in
    ignore (Unix.alarm 0);
    r
  with e ->
    ignore (Unix.alarm 0);
    raise e

let dfs ~secs d =
  with_timeout ~secs (fun () ->
      DFS.once (SearchDFS.cast_errors ~env:Builtins.env_init d))

let bfs ~secs d =
  with_timeout ~secs (fun () ->
      BFS.once (SearchBFS.cast_errors ~env:Builtins.env_init d))

let idfs ~secs d =
  with_timeout ~secs (fun () ->
      IDFS.once (SearchIDFS.cast_errors ~env:Builtins.env_init d))

let bdfs ~secs d =
  with_timeout ~secs (fun () ->
      BDFS.once (SearchBDFS.cast_errors ~env:Builtins.env_init d))

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
                   cast_size = cast_error_size result;
                   result;
                 }
         with Timeout -> TimeOut)
  |> List.mapi (fun i -> function
       | TimeOut ->
           Printf.printf "Prog %i: TIMED OUT%!\n" i;
           TimeOut
       | NoWitness ->
           Printf.printf "Prog %i: Proved No Witness%!\n" i;
           NoWitness
       | Witness _ as w ->
           Printf.printf "Prog %i: Found Witness%!\n" i;
           w)

type aggregate_search_result = {
  witness_proportion : float;
  nowitness_proportion : float;
  timeout_proportion : float;
  avg_trace_length : float;
  std_trace_length : float;
  avg_witness_size : float;
  std_witness_size : float;
  avg_cast_size : float;
  std_cast_size : float;
  witness_trace_correlation : float;
}

let aggregate_search_results rs =
  let witnesses =
    rs
    |> List.filter_map (function
         | Witness { trace_size; witness_size; cast_size; _ } ->
             Some (trace_size, witness_size, cast_size)
         | _ -> None)
  in
  let num_nowitness =
    rs |> List.filter (function NoWitness -> true | _ -> false) |> List.length
  in
  let num_timeout =
    rs |> List.filter (function TimeOut -> true | _ -> false) |> List.length
  in
  {
    witness_proportion =
      Float.of_int (List.length witnesses)
      /. Float.of_int (num_nowitness + num_timeout);
    nowitness_proportion =
      Float.of_int num_nowitness
      /. Float.of_int (List.length witnesses + num_timeout);
    timeout_proportion =
      Float.of_int num_timeout
      /. Float.of_int (num_nowitness + List.length witnesses);
    avg_trace_length =
      avg_0
        (witnesses
        |> List.map (fun (trace_length, _, _) -> Float.of_int trace_length));
    std_trace_length =
      std_0
        (witnesses
        |> List.map (fun (trace_length, _, _) -> Float.of_int trace_length));
    avg_witness_size =
      avg_0
        (witnesses
        |> List.map (fun (_, witness_size, _) -> Float.of_int witness_size));
    std_witness_size =
      std_0
        (witnesses
        |> List.map (fun (_, witness_size, _) -> Float.of_int witness_size));
    avg_cast_size =
      avg_0
        (witnesses |> List.map (fun (_, _, cast_size) -> Float.of_int cast_size));
    std_cast_size =
      std_0
        (witnesses |> List.map (fun (_, _, cast_size) -> Float.of_int cast_size));
    witness_trace_correlation =
      pearson_correlation_0
        (witnesses
        |> List.map (fun (_, witness_size, _) -> Float.of_int witness_size))
        (witnesses
        |> List.map (fun (trace_length, _, _) -> Float.of_int trace_length));
  }

let dfs_results ~secs = eval_results (dfs ~secs)
let bfs_results ~secs = eval_results (bfs ~secs)
let dfs_results ~secs = eval_results (dfs ~secs)
let idfs_results ~secs = eval_results (idfs ~secs)
let bdfs_results ~secs = eval_results (bdfs ~secs)

(* Performance Benchmarks *)
open Bechamel

let timedout = ref []

let test ~timeout ((impl_name, impl), (progn, program)) =
  let test_name = Fmt.str "%s-%i" impl_name progn in
  Test.make ~name:test_name
    (Staged.stage (fun () ->
         try Some (eval_results (impl ~secs:timeout) [ program ])
         with Timeout ->
           timedout := ("suite/" ^ test_name) :: !timedout;
           None))

let benchmark test =
  let run_bench test =
    Fmt.epr "Benchmarking %s\n%!" (Test.name test);
    let ols =
      Analyze.ols ~bootstrap:0 ~r_square:true ~predictors:Measure.[| run |]
    in
    let instances =
      Bechamel.Toolkit.Instance.
        [ monotonic_clock; minor_allocated; major_allocated ]
    in
    let cfg =
      Benchmark.cfg ~limit:100 ~quota:(Time.second 1.) ~kde:(Some 1000) ()
    in
    let raw_results = Benchmark.all cfg instances test in
    let results =
      List.map (fun instance -> Analyze.all ols instance raw_results) instances
    in
    let results = Analyze.merge ols instances results in
    (results, raw_results)
  in
  let results, _ = run_bench test in
  Fmt.pr "Timeout %a\n%!" Fmt.(list string) !timedout;
  Fmt.pr "%a@.%!"
    (Bechamel_csv.pp ~timedout:!timedout ~print_headings:true)
    results

let tests =
  let impls = [ ("dfs", dfs); ("bfs", bfs); ("idfs", idfs); ("bdfs", bdfs) ] in
  let tests =
    List.concat_map
      (fun v -> List.mapi (fun i e -> (v, (i, e))) ill_typed_annotated)
      impls
  in
  List.map (test ~timeout:1) tests |> Test.make_grouped ~name:"suite"

(* Print results *)
let print_corpus_stats cs =
  Printf.printf "Corpus Stats:\n";
  Printf.printf
    "  num_progs: %d\n\
    \  avg_prog_size: %.2f\n\
    \  std_prog_size: %.2f\n\
    \  avg_trace_size: %.2f\n\
    \  std_trace_size: %.2f\n"
    cs.num_progs cs.avg_prog_size cs.std_prog_size cs.avg_trace_size
    cs.std_trace_size

let print_aggregate_slice_size (agg : aggregate_slice_size) =
  Printf.printf "Aggregate Slice Size:\n";
  Printf.printf
    "  avg_prog_size: %.2f\n\
    \  std_prog_size: %.2f\n\
    \  avg_slice_size: %.2f\n\
    \  std_slice_size: %.2f\n\
     w_avg_proportion_prog: %.2f\n\
    \  w_std_proportion_prog: %.2f\n\
    \  w_avg_ratio_typ: %.2f\n\
    \  w_std_ratio_typ: %.2f\n"
    agg.avg_prog_size agg.std_prog_size agg.avg_slice_size agg.std_slice_size
    agg.w_avg_proportion_prog agg.w_std_proportion_prog agg.w_avg_ratio_typ
    agg.w_std_ratio_typ

let print_aggregate_cast_slice_size agg =
  Printf.printf "Aggregate Cast Slice Size:\n";
  Printf.printf
    "  avg_prog_size: %.2f\n\
    \  std_prog_size: %.2f\n\
    \  avg_term_size: %.2f\n\
    \  std_term_size: %.2f\n\
     avg_type_size: %.2f\n\
    \  std_type_size: %.2f\n\
    \  avg_slice_from_size: %.2f\n\
    \  std_slice_from_size: %.2f\n\
     avg_slice_to_size: %.2f\n\
    \  std_slice_to_size: %.2f\n\
    \  w_avg_proportion_prog: %.2f\n\
    \  w_std_proportion_prog: %.2f\n\
     w_avg_ratio_typ: %.2f\n\
    \  w_std_ratio_typ: %.2f\n"
    agg.avg_prog_size agg.std_prog_size agg.avg_term_size agg.std_term_size
    agg.avg_type_size agg.std_type_size agg.avg_slice_from_size
    agg.std_slice_from_size agg.avg_slice_to_size agg.std_slice_to_size
    agg.w_avg_proportion_prog agg.w_std_proportion_prog agg.w_avg_ratio_typ
    agg.w_std_ratio_typ

let print_aggregate_search_result res =
  Printf.printf "Aggregate Search Results:\n";
  Printf.printf
    "  witness_proportion: %.2f\n\
    \  nowitness_proportion: %.2f\n\
    \  timeout_proportion: %.2f\n\
     avg_trace_length: %.2f\n\
    \  std_trace_length: %.2f\n\
    \  avg_witness_size: %.2f\n\
    \  std_witness_size: %.2f\n\
     avg_cast_size: %.2f\n\
    \  std_cast_size: %.2f\n\
    \  witness_trace_correlation: %.2f\n"
    res.witness_proportion res.nowitness_proportion res.timeout_proportion
    res.avg_trace_length res.std_trace_length res.avg_witness_size
    res.std_witness_size res.avg_cast_size res.std_cast_size
    res.witness_trace_correlation

let print_aggregate_slice_size_expectations_error (agg_errors, agg_slices) =
  Printf.printf "Error Slice Aggregate:\n";
  Printf.printf "  Error Slice Size:\n";
  print_aggregate_slice_size agg_errors;
  Printf.printf "  Combined Slice Aggregate:\n";
  print_aggregate_slice_size agg_slices

let print_results corpus =
  print_corpus_stats (aggregate_corpus_stats corpus);
  print_endline "TYPE SLICES";
  print_endline "Type Slice Sizes: OK";
  print_aggregate_slice_size
    (aggregate_slice_sizes (slice_sizes_ok (slice_info corpus)));
  print_endline "Type Slice Sizes: Inconsistent Expectations";
  print_aggregate_slice_size_expectations_error
    (aggregate_error_slice_sizes
       (slice_sizes_incon_expectations (slice_info corpus)));
  print_endline "Type Slice Sizes: Inconsistent Branches";
  print_aggregate_slice_size_expectations_error
    (aggregate_error_slice_sizes
       (slice_sizes_incon_branches (slice_info corpus)));
  print_endline "Type Slice Sizes: Inconsistent ALL";
  print_aggregate_slice_size_expectations_error
    (aggregate_error_slice_sizes
       (slice_sizes_incon_expectations (slice_info corpus)
       @ slice_sizes_incon_branches (slice_info corpus)));
  print_endline "Type Slice Sizes: ALL";
  print_aggregate_slice_size
    (aggregate_slice_sizes (slice_sizes_all (slice_info corpus)));

  print_endline "CAST SLICES:";
  print_endline "Cast Slice Sizes: Elaborations: OK";
  print_aggregate_cast_slice_size
    (aggregate_cast_slice_sizes
       (cast_slice_sizes_ok (cast_slice_info_elaborated corpus)));
  print_endline "Cast Slice  Sizes: Elaborations: Pats";
  print_aggregate_cast_slice_size
    (aggregate_cast_slice_sizes
       (cast_slice_sizes_pats (cast_slice_info_elaborated corpus)));
  print_endline "Cast Slice  Sizes: Elaborations: ERRORS";
  print_aggregate_cast_slice_size
    (aggregate_cast_slice_sizes
       (cast_slice_sizes_errors (cast_slice_info_elaborated corpus)));
  print_endline "Cast Slice  Sizes: Elaborations: ALL";
  print_aggregate_cast_slice_size
    (aggregate_cast_slice_sizes
       (cast_slice_sizes_all (cast_slice_info_elaborated corpus)));

  print_endline "Cast Slice Sizes: Results: OK";
  print_aggregate_cast_slice_size
    (aggregate_cast_slice_sizes
       (cast_slice_sizes_ok (cast_slice_info_results corpus)));
  print_endline "Cast Slice  Sizes: Results: Pats";
  print_aggregate_cast_slice_size
    (aggregate_cast_slice_sizes
       (cast_slice_sizes_pats (cast_slice_info_results corpus)));
  print_endline "Cast Slice  Sizes: Results: ERRORS";
  print_aggregate_cast_slice_size
    (aggregate_cast_slice_sizes
       (cast_slice_sizes_errors (cast_slice_info_results corpus)));
  print_endline "Cast Slice  Sizes: Results: ALL";
  print_aggregate_cast_slice_size
    (aggregate_cast_slice_sizes
       (cast_slice_sizes_all (cast_slice_info_results corpus)))

let () =
  print_endline "WELL TYPED PROGRAMS: ";
  print_results well_typed;
  print_endline "";
  print_endline "UNANNOTATED ILL TYPED PROGRAMS: ";
  print_results ill_typed_dynamic;
  print_endline "";
  print_endline "ANNOTATED ILL TYPED PROGRAMS: ";
  print_results ill_typed_annotated;
  print_endline "";
  print_endline "ALL ILL TYPED PROGRAMS: ";
  print_results ill_typed;
  print_endline "";
  print_endline "ALL PROGRAMS: ";
  print_results all;
  print_endline "";
  print_endline "";

  print_endline "WITNESS RESULTS:";
  print_endline "DFS";
  print_aggregate_search_result
    (aggregate_search_results (dfs_results ~secs:10 ill_typed_annotated));
  print_endline "Bounded DFS";
  print_aggregate_search_result
    (aggregate_search_results (bdfs_results ~secs:10 ill_typed_annotated));
  print_endline "Interleaved DFS";
  print_aggregate_search_result
    (aggregate_search_results (idfs_results ~secs:10 ill_typed_annotated));
  print_endline "BFS";
  print_aggregate_search_result
    (aggregate_search_results (bfs_results ~secs:10 ill_typed_annotated));
  print_endline "";
  print_endline "";

  print_endline "BENCHMARKS: ";
  benchmark tests
