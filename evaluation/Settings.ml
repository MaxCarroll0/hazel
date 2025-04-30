open Haz3lcore

let settings = CoreSettings.on (* Note: search off *)

(* The current version of Hazel does not correctly parse any variable starting with "in" when inside a let expression *)
(* I delete any in[a-Z,0-9] to temporarily get around this *)
(* This must also be performed on the data, see ParseData module *)
let replace_inC = Re.(replace_string (compile (seq [ str "in"; alnum ])) ~by:"")

let ctx : Ctx.t =
  Builtins.ctx_init
  |> List.map (function
       | Ctx.VarEntry { name; id; typ } ->
           Ctx.VarEntry { name = replace_inC name; id; typ }
       | e -> e)
