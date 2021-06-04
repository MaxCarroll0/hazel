/**  Invalid Input codes:
     0 = out of fuel
     1 = free or invalid variable
     2 = ap invalid boxed function val
     3 = boxed value not a int literal 2
     4 = boxed value not a int literal 1
     5 = bad pattern match
     6 = Cast BV Hole Ground
     7 = boxed value not a float literal 1
     8 = boxed value not a float literal 2 */

[@deriving sexp]
type result =
  | InvalidInput(int)
  | BoxedValue(DHExp.t)
  | Indet(DHExp.t);

let evaluate: DHExp.t => result;
