let to_floats = List.map Float.of_int
let avg l = List.fold_left ( +. ) 0. l /. Float.of_int (List.length l)

let w_avg l w =
  List.fold_left2 (fun acc x w -> acc +. (x *. w)) 0. l w
  /. List.fold_left ( +. ) 0. w

let std l =
  let u = avg l in
  List.fold_left (fun acc x -> acc +. Float.pow (x -. u) 2.) 0. l
  /. Float.of_int (List.length l)
  |> Float.sqrt

let w_std l w =
  let u = w_avg l w in
  List.fold_left2 (fun acc x w -> acc +. (w *. Float.pow (x -. u) 2.)) 0. l w
  /. List.fold_left ( +. ) 0. w
  |> Float.sqrt

(* Ignoring 0 results *)
let avg_0 l = l |> List.filter (( != ) 0.) |> avg

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

let std_0 l = l |> List.filter (( != ) 0.) |> std

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

let pearson_correlation xs ys =
  let avg_x = avg xs in
  let avg_y = avg ys in
  let covariance, var_x, var_y =
    List.fold_left2
      (fun (cov, vx, vy) x y ->
        let dx = x -. avg_x in
        let dy = y -. avg_y in
        (cov +. (dx *. dy), vx +. (dx *. dx), vy +. (dy *. dy)))
      (0.0, 0.0, 0.0) xs ys
  in
  if var_x = 0.0 || var_y = 0.0 then 0.0
    (* If no variation then correlation undefined *)
  else covariance /. sqrt (var_x *. var_y)

let pearson_correlation_0 xs ys =
  pearson_correlation (List.filter (( > ) 0.) xs) (List.filter (( > ) 0.) ys)
