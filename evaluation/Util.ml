let to_floats = List.map Float.of_int
let avg l = List.fold_left ( +. ) 0. l /. Float.of_int (List.length l)

let w_avg l w =
  List.fold_left2 (fun acc x w -> acc +. (x *. w)) 0. l w
  /. List.fold_left ( +. ) 0. w

let std l =
  let u = avg l in
  List.fold_left (fun acc x -> acc +. x -. u) 0. l
  /. Float.of_int (List.length l)
  |> Float.sqrt

let w_std l w =
  let u = w_avg l w in
  List.fold_left2 (fun acc x w -> acc +. (w *. (x -. u))) 0. l w
  /. List.fold_left ( +. ) 0. w
  |> Float.sqrt

(* Ignoring 0 results *)
let avg_0 l = l |> List.filter (( = ) 0.) |> avg

let w_avg_0 l w =
  let rec filter = function
    | [], _ -> ([], [])
    | _ :: _, [] -> ([], [])
    | x :: xs, _ :: ws when x = 0. -> filter (xs, ws)
    | x :: xs, w :: ws ->
        let xs, ws = filter (xs, ws) in
        (x :: xs, w :: ws)
  in
  filter (l, w) |> fun (l, w) -> w_avg l w

let std_0 l = l |> List.filter (( = ) 0.) |> std

let w_std_0 l w =
  let rec filter = function
    | [], _ -> ([], [])
    | _ :: _, [] -> ([], [])
    | x :: xs, _ :: ws when x = 0. -> filter (xs, ws)
    | x :: xs, w :: ws ->
        let xs, ws = filter (xs, ws) in
        (x :: xs, w :: ws)
  in
  filter (l, w) |> fun (l, w) -> w_std l w
