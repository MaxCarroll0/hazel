type status =
  | Solved(ITyp.t)
  | Unsolved(PotentialTypeSet.t);

type t = (ITyp.t, status);

type type_hole_to_solution = Hashtbl.t(Id.t, status);

type global_inference_info = {
  enabled: bool,
  solution_statuses: type_hole_to_solution,
};

type suggestion('a) =
  | Solvable('a)
  | NestedInconsistency('a)
  | NoSuggestion(reason_for_silence)
and reason_for_silence =
  | SuggestionsDisabled
  | NonTypeHoleId
  | OnlyHoleSolutions
  | InconsistentSet;

let get_suggestion_text_for_id =
    (id: Id.t, global_inference_info: global_inference_info)
    : suggestion(string) => {
  print_endline("in get suggestion text for id " ++ Id.to_string(id));
  if (global_inference_info.enabled) {
    let status_opt =
      Hashtbl.find_opt(global_inference_info.solution_statuses, id);
    switch (status_opt) {
    | Some(Solved(Unknown(_))) =>
      print_endline("No Suggestion only holes");
      NoSuggestion(OnlyHoleSolutions);
    | Some(Solved(ityp)) =>
      print_endline("suggestion solved as a single type");
      let typ_to_string = x => Typ.typ_to_string(x, false);
      Solvable(ityp |> ITyp.ityp_to_typ |> typ_to_string);
    | Some(Unsolved([potential_typ])) =>
      print_endline("Suggestion unsolved as a single type");
      NestedInconsistency(
        PotentialTypeSet.string_of_potential_typ(false, potential_typ),
      );
    | Some(Unsolved(_)) =>
      print_endline("No suggestion unsolved as many");
      NoSuggestion(InconsistentSet);
    | None =>
      print_endline("No Suggestion non type hole id");
      NoSuggestion(NonTypeHoleId);
    };
  } else {
    print_endline("No suggestion disabled");
    NoSuggestion(SuggestionsDisabled);
  };
};

let hole_nib: Nib.t = {shape: Convex, sort: Any};
let hole_mold: Mold.t = {out: Any, in_: [], nibs: (hole_nib, hole_nib)};

let empty_solutions = (): type_hole_to_solution => Hashtbl.create(20);

let mk_global_inference_info = (enabled, annotations) => {
  {enabled, solution_statuses: annotations};
};

let empty_info = (): global_inference_info =>
  mk_global_inference_info(true, empty_solutions());

let get_desired_solutions =
    (inference_results: list(t)): type_hole_to_solution => {
  let id_and_status_if_type_hole = (result: t): option((Id.t, status)) => {
    switch (result) {
    | (Unknown(TypeHole(id)), status) => Some((id, status))
    | _ => None
    };
  };

  let elts = List.filter_map(id_and_status_if_type_hole, inference_results);
  let new_map = Hashtbl.create(List.length(elts));

  List.iter(((id, annot)) => Hashtbl.add(new_map, id, annot), elts);

  new_map;
};

let condense =
    (potential_typ_set: MutablePotentialTypeSet.t, key: ITyp.t): status => {
  let (potential_typ_set, err) =
    MutablePotentialTypeSet.snapshot_class(potential_typ_set, key);
  let sorted_potential_typ_set =
    PotentialTypeSet.sort_potential_typ_set(potential_typ_set);

  let filtered_potential_typ_set =
    PotentialTypeSet.filter_unneeded_holes(
      PotentialTypeSet.is_known,
      sorted_potential_typ_set,
    );

  switch (err) {
  | Some(_) => Unsolved(filtered_potential_typ_set)
  | None =>
    let solved_opt =
      PotentialTypeSet.filtered_potential_typ_set_to_typ(
        filtered_potential_typ_set,
      );
    switch (solved_opt) {
    | Some(typ) => Solved(typ)
    | None => Unsolved(filtered_potential_typ_set)
    };
  };
};

let rec prov_to_priority = (prov: Typ.type_provenance): string => {
  switch (prov) {
  | NoProvenance => ""
  | ExpHole(_, id)
  | TypeHole(id) => Id.to_string(id)
  | Matched(_, prov) => prov_to_priority(prov)
  };
};

let rec convert_leftmost_to_priority = (typ: ITyp.t): string => {
  switch (typ) {
  | Int
  | Unit
  | Float
  | String
  | Bool => ""
  | Unknown(prov) => prov_to_priority(prov)
  | List(elt_typ) => convert_leftmost_to_priority(elt_typ)
  | Arrow(typ_lhs, typ_rhs)
  | Prod(typ_lhs, typ_rhs)
  | Sum(typ_lhs, typ_rhs) =>
    let lhs = convert_leftmost_to_priority(typ_lhs);
    let rhs = convert_leftmost_to_priority(typ_rhs);
    switch (lhs, rhs) {
    | ("", "") => ""
    | ("", _) => rhs
    | _ => lhs
    };
  };
};

let comp_results = ((ty1, _): t, (ty2, _): t): int => {
  let priority1 = convert_leftmost_to_priority(ty1);
  let priority2 = convert_leftmost_to_priority(ty2);
  Stdlib.compare(priority1, priority2);
};
