# REFACTOR:

## TypeSlice.re: 
As compared to `witnesses` branch:
- Remove Join type
- Add global code slice and context slices (for ana context): `type slice_ana'
- Impement in direct correspondance to Slice.re
- Implement as a SUM TYPE. With empty slices being exactly as Typ.
- Therefore: we can combine slices and types in the same tree to get partially computed slices.

## Use of Typ:
Replace all use of Typ with TypSlice. 

## Self.re:
- All logic for synthesis slices

## Mode.re:
- All logic for analysis slices

## Info.re:
- Add slice-related functions. i.e. getting synthesis slice, analysis slice (from mode and self respectively) and fixed slice (real `Info.ty`).
- Also add slice with no syn switches??

## Statics.re & Elaboration.re:
- Separate use of Slice.re to allow disabling. i.e. using Typ only.
- Do this by passing a bool through (also allows calculating slices only for specific _regions_ of the code: which could be linked directly to the )
- Create a (let.slice) binding operator which skips slice computation dependent on bool. _Make sure this still maintains the old slice!_
