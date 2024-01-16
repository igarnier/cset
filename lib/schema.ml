(* A schema in the sense of the paper "Categorical data structures for technical computing"  *)
type t =
  { name : string;  (** The name of the schema *)
    objects : string array;  (** The objects of the category *)
    attrs : string array;  (** The attributes of the objects of the category *)
    morphisms : (string * int * int) array;
        (** Triples [(name, source, target)]. Identities are not represented.
            The source and target are the indices of the objects in the [objects] array.  *)
    attr_morphisms : (string * int * int) array
        (** Triples [(name, source, target)]. The source is an index in the [objects] array, while
            [target] is an index in the [attrs] array. *)
  }

let to_dot fmtr { name; objects; attrs; morphisms; attr_morphisms } =
  let open Format in
  let open Fmt in
  let pp_list sep pp_elt = array ~sep pp_elt in
  let pp_array sep pp_elt = pp_list sep pp_elt in
  let pp_morphism fmtr (name, src, tgt) =
    fprintf fmtr "%s -> %s [label=\"%s\"]" objects.(src) objects.(tgt) name
  in
  let pp_attr_morphism fmtr (name, src, tgt) =
    fprintf fmtr "%s -> %s [label=\"%s\"]" objects.(src) attrs.(tgt) name
  in
  fprintf
    fmtr
    "@[<v 2>digraph %s {@,%a@,%a@,%a@,%a@,}@]"
    name
    (pp_array Fmt.cut string)
    (Array.map (fun s -> s ^ " [shape=box]") objects)
    (pp_array Fmt.cut string)
    (Array.map (fun s -> s ^ " [shape=circle]") attrs)
    (pp_array Fmt.cut pp_morphism)
    morphisms
    (pp_array Fmt.cut pp_attr_morphism)
    attr_morphisms

let option_get o =
  [%meta
    if Sys.ocaml_version >= "4.08.0" then [%e Option.get o]
    else [%e match o with None -> invalid_arg "option_get" | Some x -> x]]

let%test_module "Schema" =
  (module struct
    let%expect_test "to_dot" =
      let schema =
        { name = "weighted_digraph";
          objects = [| "V"; "E" |];
          attrs = [| "W" |];
          morphisms = [| ("s", 1, 0); ("t", 1, 0) |];
          attr_morphisms = [| ("w", 1, 0) |]
        }
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
