# Type Slicing
## Interaction
Display type slices after clicking on the type in the cursor inspector. Allowing a slice of the synthesised type AND/OR the slice of the analytic type (if in analysis mode).
Colour slices into three divisions:
- Context Slice: Slice of the origins of type assumptions used in a slice. If type context is generalised to _slice context_ then shade diferently the: name binding slices (of the pattern directly) and definition slices (of the defined expression).
- Analysis Slice: The slice of the analytic type imposed on the selected expression
- Synthesis Slice: The main slice of a synthesised type
Currently the Synthesis slices follow the theory. However, _analysis slices are somewhat ad-hoc_ in that the version replacing holes for un-marked areas is not always syntactically valid and the 'scope' (overall expression to consider filling) of the analysed type is unclear/ad-hoc.
Some analysis types (lists, annotations) are closer to being 'correct'. 

- Consider two versions for analysis slices. The more verbose the theoretically correct/syntactically valid slices as compared to simpler ad-hoc slices which may be more readable. Maybe store structural parts separately (also more efficient) and colour them more softly.

## Theory
- **Type Analysis Slices.** In particular, what does it mean to provide a slice of the _syntactic context_ that some expression is used in -- consider the case of a cons 1::2::[]
- Explicit Joins (not required for Core Hazel)
- Lists (not required for Core Hazel) -- useful to prove the implementation slices for lists is correct/makes sense
- Decomposed slice of a function argument type

## Implementation
### Core 
- Correctly managing SynSwitch slices -- Decompose syn slice, replacing synswitches in ana slice with the corresponding syn slices.
- Make the context slices involving annotations maintain the structure of the type (currently : Int -> Int). May actually be more readable as is.
- Make context slice non-incremental and remove entries accordingly when bound (currently not done). Or alternatively keep incremental but have a second explicit 'bound' context (could be achieved with a Coctx.t).
- full\_slice: Keep slices of only most specific join branches if consistent.
- Keep slice of binding definitions when in syn mode. (Intuitively useful, but not theoretically required). Similarly, keep pattern slices. Easiest way to do this would be to generalise type context to a _slice context_.
- Type slicing for: type aliases, constructors, fix/recursion.

### Extension
- Type slicing for patterns & match.
- Type slicing for type variables and type functions.
- (low priority) Type slicing for: unquote, tests, filters, deferred applications, 

## Architecture
- Shift parts of slice calculation to Info.re, using Self and Mode. In particular, add to Info.derived\_exp and Info.derived\_pat.
- Also shift relevant parts to Self.re and Mode.re
- Distinguish between Synthesised and Analytic type slices and allow both to be retrieved from Info.t clearly and cleanly.
- Better integration between Typ.re and Slice.re. 
- Distinguish better the idea of a decomposable slice indexed by a type (Slice.t) and a concrete code slice (Slice.slice). In particular, both are confusingly referred to in code as 's' or 'slice'. Maybe pack Slice.slice related code into a sub-module 'Code'.

## Efficient & Performance
- Slices can hold lods of duplicate ids. Think of a better way to represent these while maintaining decomposability.
- Appears to be too slow for live code completion. Add a filter to not compute slices, and only compute upon request (e.g. clicking on syn/ana type in the cursor inspector). For the moment turning off assist gives good performance.

# Cast Slicing
## Interaction
Click on casts in the stepper to retrieve relevant type slices.

## Implementation
(everything)

# Search Procedure
(everything)


# Bugs
- Elaboration inserts too many casts during let expressions (see test elaboration3).
- Let bindings annotated with single `?` fail to pick up correct context slice: `let f : ? = ? in f`
- Type checking (only the name checking & placing into error holes) for sum types/type aliases is broken by incorrect use of Slice.temp 
