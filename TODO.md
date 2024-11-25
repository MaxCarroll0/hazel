# Type Slicing
## Interaction
Display type slices after clicking on the type in the cursor inspector. Allowing a slice of the synthesised type AND/OR the slice of the analytic type (if in analysis mode).
Colour slices into three divisions:
- Context Slice: Slice of the origins of type assumptions used in a slice. If type context is generalised to slice context then shade diferently the: name binding slices (of the pattern directly) and definition slices (of the defined expression).
- Analysis Slice: The slice of the analytic type imposed on the selected expression
- Synthesis Slice: The main slice of a synthesised type

## Theory
- Type Analysis Slices. In particular, what does it mean to provide a slice of the _syntactic context_ that some expression is used in -- consider the case of a cons 1::2::[]
- Explicit Joins (not required for Core Hazel)
- Lists (not required for Core Hazel) -- useful to prove the implementation slices for lists is correct/makes sense
- Decomposed slice of a function argument type

## Implementation
### Core 
- Correctly managing SynSwitch slices -- Decompose syn slice, replacing synswitches in ana slice with the corresponding syn slices.
- Keep track of ids of annotations used in context slices when relevant.
- Make context slice non-incremental and remove entries accordingly when bound (currently not done). Or alternatively keep incremental but have a second explicit 'bound' context (could be achieved with a Coctx.t).
- full\_slice: Keep slices of only most specific join branches if consistent.
- Keep slice of binding definitions when in syn mode. (Intuitively useful, but not theoretically required). i.e. Generalise type context to a slice context.
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

## Bugs
- Moving cursor quickly is very slow with slicing. Slices should not be recalculated (besides compiling a full slice) upon cursor movement, so why is performance degraded?

# Cast Slicing
## Interaction
Click on casts in the stepper to retrieve relevant type slices.

## Implementation
(everything)

# Search Procedure
(everything)
