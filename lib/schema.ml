(* A schema in the sense of the paper "Categorical data structures for technical computing"  *)
type t =
  { name : string;  (** The name of the schema *)
    objects : string array;  (** The objects of the category *)
    attrs : string array;  (** The attributes of the objects of the category *)
    morphisms : (string * int * int) array;
        (** Triples [(name, source, target)]. Identities are not represented.
            The source and target are the indices of the objects in the [objects] array.  *)
    attr_morphisms : (string * int * int) array;
        (** Triples [(name, source, target)]. The source is an index in the [objects] array, while
            [target] is an index in the [attrs] array. *)
    objects_table : (string, int) Hashtbl.t;
    attrs_table : (string, int) Hashtbl.t;
    morphisms_table : (string, int) Hashtbl.t;
    attr_morphisms_table : (string, int) Hashtbl.t
  }

type Error_monad.error += Invalid_schema of string

let () =
  Error_monad.register
    (function Invalid_schema err -> Some err | _ -> None)
    (fun fmtr err -> Format.fprintf fmtr "Invalid schema: %s" err)

let invalid_schema err = Error_monad.fail (Invalid_schema err)

let all_distinct f array =
  let n = Array.length array in
  let table = Hashtbl.create n in
  let rec loop i =
    if i = n then true
    else
      let elt = f array.(i) in
      if Hashtbl.mem table elt then false
      else (
        Hashtbl.add table elt i ;
        loop (i + 1))
  in
  let success = loop 0 in
  if success then Some table else None

let get_table f array error_msg =
  match all_distinct f array with
  | None -> invalid_schema error_msg
  | Some table -> Error_monad.return table

let in_bounds i a = 0 <= i && i < Array.length a

let make ~name ~objects ~attrs ~morphisms ~attr_morphisms =
  let open Error_monad in
  let* objects_table = get_table Fun.id objects "objects must be distinct" in
  let* attrs_table = get_table Fun.id attrs "attributes must be distinct" in
  let* morphisms_table =
    get_table (fun (m, _, _) -> m) morphisms "morphisms must be distinct"
  in
  let* attr_morphisms_table =
    get_table
      (fun (a, _, _) -> a)
      attr_morphisms
      "attribute morphisms must be distinct"
  in
  let* () =
    if
      not
        (Array.for_all
           (fun (_, s, t) -> in_bounds s objects && in_bounds t objects)
           morphisms)
    then invalid_schema "morphisms must be between objects"
    else return ()
  in
  let* () =
    if
      not
        (Array.for_all
           (fun (_, s, t) -> in_bounds s objects && in_bounds t objects)
           attr_morphisms)
    then invalid_schema "attribute morphisms must be between objects"
    else return ()
  in
  return
    { name;
      objects;
      attrs;
      morphisms;
      attr_morphisms;
      objects_table;
      attrs_table;
      morphisms_table;
      attr_morphisms_table
    }

let internal_get_object schema object_name =
  match Hashtbl.find_opt schema.objects_table object_name with
  | None -> Format.kasprintf invalid_schema "object %s not found" object_name
  | Some index -> Ok index

let internal_get_morphism schema morphism_name =
  match Hashtbl.find_opt schema.objects_table morphism_name with
  | None ->
      Format.kasprintf invalid_schema "morphism %s not found" morphism_name
  | Some index -> Ok index

let hom schema src tgt =
  let open Error_monad in
  let* src = internal_get_object schema src in
  let* tgt = internal_get_object schema tgt in
  let morphisms =
    Array.to_seq schema.morphisms
    |> Seq.filter_map (fun (m, s, t) ->
           if s = src && t = tgt then Some m else None)
    |> Array.of_seq
  in
  return morphisms

let hom_out schema src =
  let open Error_monad in
  let* src = internal_get_object schema src in
  let morphisms =
    Array.to_seq schema.morphisms
    |> Seq.filter_map (fun (m, s, _t) -> if s = src then Some m else None)
    |> Array.of_seq
  in
  return morphisms

let to_dot fmtr { name; objects; attrs; morphisms; attr_morphisms; _ } =
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

module Location = Location

type column = int Containers.Vector.vector

let record_type_of_obj schema obj =
  let open Ppxlib in
  let morphisms = Error_monad.run (hom_out schema obj) in
  Ptype_record
    (List.map
       (fun m ->
         { pld_name = Loc.make ~loc:!Ast_helper.default_loc m;
           pld_mutable = Mutable;
           pld_type = [%type: column];
           pld_loc = Location.none;
           pld_attributes = []
         })
       (Array.to_list morphisms))

(* let build_data_frame loc schema obj = *)
(*   let (module Builder) = Ppxlib.Ast_builder.make loc in *)
(*   let open Builder in *)
(*   pstr_type *)
(*     Nonrecursive *)
(*     [ type_declaration *)
(*         ~name:(Located.mk (Longident.Lident schema.name)) *)
(*         ~params:[] *)
(*         ~cstrs:[] *)
(*         ~kind: *)
(*           (Ptype_record *)
(*              (List.map *)
(*                 (fun s -> (s, Mutable, [%type: float])) *)
(*                 (Array.to_list schema.attrs))) *)
(*         ~private_:Public ] *)

(* [%%meta *)
(* record_type_of_obj schema "V" |> function Ok x -> x | _ -> assert false] *)

(* let option_get o = *)
(*   [%meta *)
(*     if Sys.ocaml_version >= "4.08.0" then [%e Option.get o] *)
(*     else [%e match o with None -> invalid_arg "option_get" | Some x -> x]] *)

