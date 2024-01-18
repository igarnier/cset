[%%metapackage cset]

[%%meta
let schema =
  let open Cset in
  Error_monad.run
  @@ Schema.make
       ~name:"weighted_digraph"
       ~objects:[| "V"; "E" |]
       ~attrs:[| "W" |]
       ~morphisms:[| ("s", 1, 0); ("t", 1, 0) |]
       ~attr_morphisms:[| ("w", 1, 0) |]
in
Cset.Schema.typedecl_of_obj schema "E" ]
