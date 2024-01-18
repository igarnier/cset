open Cset
open Schema

let%test_module "Schema" =
  (module struct
    let%expect_test "to_dot" =
      let schema =
        Error_monad.run
        @@ make
             ~name:"weighted_digraph"
             ~objects:[| "V"; "E" |]
             ~attrs:[| "W" |]
             ~morphisms:[| ("s", 1, 0); ("t", 1, 0) |]
             ~attr_morphisms:[| ("w", 1, 0) |]
      in
      to_dot Format.std_formatter schema ;
      [%expect
        {|
          digraph weighted_digraph {
            V [shape=box]
            E [shape=box]
            W [shape=circle]
            E -> V [label="s"]
            E -> V [label="t"]
            E -> W [label="w"]
            } |}]
  end)
