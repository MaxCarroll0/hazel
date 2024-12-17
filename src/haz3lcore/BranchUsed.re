[@deriving show]
type t =
  | None
  | Left
  | Right
  | Both;

let left =
  fun
  | Left
  | Both => true
  | _ => false;
let right =
  fun
  | Right
  | Both => true
  | _ => false;

let flip =
  fun
  | Left => Right
  | Right => Left
  | x => x;

let combine_branches_used = (branch_used1, branch_used2) =>
  switch (branch_used1, branch_used2) {
  | (Both, _)
  | (_, Both)
  | (Left, Right)
  | (Right, Left) => Both
  | (Left, _)
  | (_, Left) => Left
  | (Right, _)
  | (_, Right) => Right
  | (None, None) => None
  };
