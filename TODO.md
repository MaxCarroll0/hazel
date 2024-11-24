# Type Slicing
## Interaction
Display type slices after clicking on the type in the cursor inspector. Allowing a slice of the synthesised type AND/OR the slice of the analytic type (if in analysis mode).
Colour slices into three divisions:
- Context Slice: Slice of the origins of type assumptions used in a slice
- Analysis Slice: The slice of the analytic type imposed on the selected expression
- Synthesis Slice: The main slice of a synthesised type

## Theory
- Type Analysis Slices. In particular, what does it mean to provide a slice of the _syntactic context_ that some expression is used in -- consider the case of a cons 1::2::[]
- Explicit Joins (not required for Core Hazel)
- Lists (not required for Core Hazel) -- useful to prove the implementation slices for lists is correct/makes sense

## Implementation
### Core 
- Correctly managing SynSwitch slices
- Context slices. 
- Type slicing for: functions, bindings, type aliases, sum types, fix/recursion.

### Extension
- Type slicing for patterns & match.
- Type slicing for type variables and type functions.
- (low priority) Type slicing for: unquote, tests, filters, deferred applications, 

## Architecture
- Distinguish between Synthesised and Analytic type slices and allow both to be retrieved from Info.t clearly and cleanly.
- Better integration between Typ.re and Slice.re. 
- Distinguish better the idea of a decomposable slice indexed by a type (Slice.t) and a concrete code slice (Slice.slice). In particular, both are confusingly referred to in code as 's' or 'slice'. Maybe pack Slice.slice related code into a sub-module 'Code'.
- Shift slice calculation to Mode.re

## Bugs


# Cast Slicing
## Interaction
Click on casts in the stepper to retrieve relevant type slices.

## Implementation
(everything)

# Search Procedure
(everything)
