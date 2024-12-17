# REFACTOR:

## TypeSlice.re: 
As compared to `witnesses` branch:
- [x] Remove Join type
- [x] Add global code slice and context slices (for ana context): `type slice_ana'
- [x] Impement in direct correspondance to Slice.re
- [x] Implement as a SUM TYPE. With empty slices being exactly as Typ.
- [x] Therefore: we can combine slices and types in the same tree to get partially computed slices.
- [ ] Add syntactic destructuring functions (see in Unboxing.re) to TypSlice.re
- [ ] Simplify and remove redundancy from join_using
- [ ] Remove slc_incr
- [ ] Preserve global slices on variables during substitution

## Use of Typ:
- [x] Replace all (relevant) use of Typ with TypSlice.
- [ ] Decide on use of Typ vs TypSlice in constructor maps

## Self.re:
- [x] Term id logic for synthesis slices
- [x] Ctx slice logic for synthesis slices
- [ ] Display ctx slices.

## Mode.re:
- [-] Term id logic for analysis slices
- [ ] Ctx slice logic for analysis slices

## Info.re:
- [ ] Add slice-related functions. i.e. getting synthesis slice, analysis slice (from mode and self respectively) and fixed slice (real `Info.ty`).
- [ ] Also add slice with no syn switches??
- [ ] Calculate ctx_used in typslices from Co_ctx and Ctx?

## Statics.re & Elaboration.re:
- [ ] Separate use of Slice.re to allow disabling. i.e. using Typ only. Could do this by passing a bool through (also allows calculating slices only for specific _regions_ of the code: which could be linked directly to the). Or could create a (let.slice) binding operator which skips slice computation dependent on bool. _Make sure this still maintains the old slice!_
- [ ] Separate Self.annot into utyp\_to\_info\_map

## Parsing
- [ ] Make decision on full use of TypSlice or use of Typ for this stage
- [ ] Attach slices correctly to types/casts in this stage if using TypSlice.

## Other
- [ ] Remove redundancy in uinfo_of_typ and also consider using Typ.t for Info.typ
- [ ] More ergonomic use of TypSlice (better versions of map & apply)
- [ ] Improve performance: Likely issues due to overuse of TypSlice.typ_of? Hopefully not due to TypSlice.wrap_global or wrap_incr.....

## Bugs
