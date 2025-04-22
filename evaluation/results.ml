open Haz3lcore
open ParseData
open SlicingUtil

let settings = CoreSettings.on (* Note: search off *)

(* Effectiveness *)
(* Type Slice Size Stats*)
let slice_info =
  ill_typed @ well_typed
  |> List.map (fun e -> slice_info (Statics.mk settings Builtins.ctx_init e) e)
  |> List.flatten

(* Total slice size info *)
type slice_size = { term_type_size : int; slice_size : int; proportion : float }

let slice_sizes_all =
  slice_info
  |> List.map (fun (_, term, slice, _) ->
         (term_size term, term_size (TypSlice slice), slice_size slice))
  |> List.map (fun (term_size, type_size, slice_size) ->
         {
           term_type_size = term_size + type_size;
           slice_size;
           proportion =
             Float.of_int slice_size /. Float.of_int (term_size + type_size);
         })

let slice_sizes_ok =
  slice_info
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

let slice_sizes_incon_expectations =
  slice_info
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

let slice_sizes_incon_branches =
  slice_info
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
