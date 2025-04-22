open Haz3lcore
open ParseData
open SlicingUtil

let settings = CoreSettings.on (* Note: search off *)

(* Effectiveness *)
(* Type Slice Size Stats*)
let slice_info =
  ParseData.ill_typed @ ParseData.well_typed
  |> List.map (fun e ->
         SlicingUtil.slice_info (Statics.mk settings Builtins.ctx_init e) e)
  |> List.flatten

(* Total slice size info *)
type slice_size = { term_size : int; slice_size : int; proportion : float }

let slice_sizes_all =
  slice_info
  |> List.map (fun (_, term, slice, _) ->
         (SlicingUtil.term_size term, SlicingUtil.slice_size slice))
  |> List.map (fun (term_size, slice_size) ->
         {
           term_size;
           slice_size;
           proportion = Float.of_int slice_size /. Float.of_int term_size;
         })

let slice_sizes_ok =
  slice_info
  |> List.filter (function _, _, _, NoTypeError -> true | _ -> false)
  |> List.map (fun (_, term, slice, _) ->
         (SlicingUtil.term_size term, SlicingUtil.slice_size slice))
  |> List.map (fun (term_size, slice_size) ->
         {
           term_size;
           slice_size;
           proportion = Float.of_int slice_size /. Float.of_int term_size;
         })

(* Slice size vs the combined slice size of the expectations*)
type slice_size_expectations_error = {
  slice_size : slice_size;
  expectations_slice_size : slice_size;
}

let inconsistent_slice_sizes =
  slice_info
  |> List.filter_map (function
       | _, term, slice, Inconsistent { syn; ana } ->
           Some
             ( SlicingUtil.term_size term,
               SlicingUtil.slice_size slice,
               SlicingUtil.slice_size syn + SlicingUtil.slice_size ana )
       | _ -> None)
  |> List.map (fun (term_size, slice_size, expectations_slice_size) ->
         {
           slice_size =
             {
               term_size;
               slice_size;
               proportion = Float.of_int slice_size /. Float.of_int term_size;
             };
           expectations_slice_size =
             {
               term_size;
               slice_size = expectations_slice_size;
               proportion =
                 Float.of_int expectations_slice_size /. Float.of_int term_size;
             };
         })
