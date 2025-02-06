# Plan for Search Procedure Implementation:
ONLY consider closed terms, so that unbound variables are not a problem.
1) [ ] Add metavariables to holes and propagate accordingly
2) [ ] Implement hole substitution via metavariable
3) [ ] Make Indet terms track the holes that are required to be instantiated to make progress.
Either: Do this for all Indet terms OR deconstruct and Indet term and find (in partnership with below)
4) [ ] Reconstruct required types for holes in instantiation via Casts. 
5) [ ] Implement non-deterministic value instantiation of any type with a total ordering. (i.e. via Lazy List generators). Note: These instatiations may (should, where possible) contain holes. Consdier using QuickCheck
6) [ ] (Optional) Impelement random value instantiation (i.e. shuffling the lazy lists above somehow)
7) [ ] Implement type witness procedure by connecting the above components.
8) [ ] Add a stepper UI for such instantiations with start point given by cursor.
9) [ ] (Optional) Distinguish types of holes by their origin (static error, dynamic error, user-inserted, instantiated by search procedure)
10) [ ] (Optional) Use analysis slice contexts to extend these type witnesses to actually give Type error witnesses/cast errors