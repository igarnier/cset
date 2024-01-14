type schema = { objects : string array; morphisms : (int * int) array }

(*
  A set-valued functor on a schema is the data of:
  - a Fin.fin for each object (set of objects returned by the functor,
    each represented as the set { 0, 1, ..., n-1 })
  - a Fin.fun for each morphism?
  - Indexation is a table reversing a Fin.fun
  The outcome is a bunch of columns:
  - for each morphism, a column of sets of objects
*)
