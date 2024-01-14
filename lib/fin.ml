type fin = Fin of int [@@ocaml.unboxed]

let equal_fin (Fin i1) (Fin i2) = i1 = i2

let pp_fin fmtr (Fin i) = Format.fprintf fmtr "Fin(%d)" i

module Fun : sig
  type t = private { dom : fin; codom : fin; values : int array }

  val make : values:int array -> codom:fin -> t Error_monad.t

  val pp : Format.formatter -> t -> unit

  val pp_full : Format.formatter -> t -> unit
end = struct
  type t = { dom : fin; codom : fin; values : int array }

  type reason = Invalid_values

  type Error_monad.error += Finite_function of reason

  let () =
    Error_monad.register
      (function
        | Finite_function Invalid_values -> Some Invalid_values | _ -> None)
      (fun fmtr Invalid_values ->
        Format.fprintf fmtr "Fin.Fun.make: invalid values")

  let make ~values ~codom =
    let open Error_monad in
    let dom = Fin (Array.length values) in
    let (Fin cdm) = codom in
    if not (Array.for_all (fun v -> 0 <= v && v < cdm) values) then
      fail (Finite_function Invalid_values)
    else return { dom; codom; values }

  let pp fmtr { dom; codom; values = _ } =
    Format.fprintf fmtr "%a => %a" pp_fin dom pp_fin codom

  let pp_full fmtr { dom; codom; values } =
    Format.fprintf
      fmtr
      "[%a]: %a => %a"
      (Format.pp_print_list
         ~pp_sep:(fun fmtr () -> Format.fprintf fmtr ",")
         Format.pp_print_int)
      (Array.to_list values)
      pp_fin
      dom
      pp_fin
      codom
end

module Parallel_pair : sig
  type t = private { f1 : Fun.t; f2 : Fun.t }

  val make : Fun.t -> Fun.t -> t Error_monad.t

  val coequalize : t -> (Fun.t * fin) Error_monad.t
end = struct
  type t = { f1 : Fun.t; f2 : Fun.t }

  type Error_monad.error += Invalid_pair of Fun.t * Fun.t

  let () =
    Error_monad.register
      (function Invalid_pair (f1, f2) -> Some (f1, f2) | _ -> None)
      (fun fmtr (f1, f2) ->
        Format.fprintf
          fmtr
          "Fin.Fun.make: invalid pair (%a, %a)"
          Fun.pp
          f1
          Fun.pp
          f2)

  let make (f1 : Fun.t) (f2 : Fun.t) =
    let open Error_monad in
    if not (equal_fin f1.dom f2.dom && equal_fin f1.codom f2.codom) then
      fail (Invalid_pair (f1, f2))
    else return { f1; f2 }

  module Vec = CCVector

  type eq_class = { uid : int; elts : int list }

  let make_singleton_class =
    let c = ref 0 in
    fun x ->
      let v = !c in
      incr c ;
      { uid = v; elts = [x] }

  let merge cl1 cl2 =
    if cl1.uid = cl2.uid then cl1
    else if cl1.uid < cl2.uid then
      { uid = cl1.uid; elts = List.rev_append cl1.elts cl2.elts }
    else { uid = cl2.uid; elts = List.rev_append cl1.elts cl2.elts }

  let coequalize { f1; f2 } =
    let open Error_monad in
    let (Fin dom) = f1.dom in
    let (Fin codom) = f2.codom in
    let roots =
      Array.init codom (fun i -> Uf.create (make_singleton_class i))
    in
    let rec loop i bound =
      if i >= bound then return ()
      else
        let x1 = roots.(f1.values.(i)) in
        let x2 = roots.(f2.values.(i)) in
        let* () = Uf.join x1 x2 (fun cl1 cl2 -> merge cl1 cl2 |> return) in
        loop (i + 1) bound
    in
    let* () = loop 0 dom in
    (* The cardinality of the coequalizer object is the number of classes.
       The coequalizing map associates each element of a class to the index of a class.
       Indices are assigned to classes arbitrarily.
    *)
    let distinct_classes = Hashtbl.create 101 in
    let distinct_classes_count = ref 0 in
    Array.iter
      (fun clss ->
        let clss = Uf.get clss in
        if Hashtbl.mem distinct_classes clss.uid then ()
        else (
          Hashtbl.add distinct_classes clss.uid !distinct_classes_count ;
          incr distinct_classes_count))
      roots ;
    let coeq_object = Fin !distinct_classes_count in
    let values =
      Array.init codom (fun i ->
          let clss = Uf.get roots.(i) in
          Hashtbl.find distinct_classes clss.uid)
    in
    let* coeq_morphism = Fun.make ~values ~codom:coeq_object in
    return (coeq_morphism, coeq_object)
end

(* Tests *)

let%test_module "Fin" =
  (module struct
    let%test "equal_fin" = equal_fin (Fin 1) (Fin 1)

    let%test "not equal_fin" = not (equal_fin (Fin 1) (Fin 2))

    let%test "pp_fin" = Format.asprintf "%a" pp_fin (Fin 1) = "Fin(1)"
  end)

let%test_module "Fun" =
  (module struct
    let%expect_test "make" =
      let open Error_monad in
      run
        (let* f = Fun.make ~values:[| 0; 1; 0 |] ~codom:(Fin 2) in
         Format.printf "%a@." Fun.pp f ;
         return ()) ;
      [%expect {| Fin(3) => Fin(2) |}]

    let%expect_test "make_error" =
      let open Error_monad in
      (match
         let* _f = Fun.make ~values:[| 0; 2; 0 |] ~codom:(Fin 2) in
         return ()
       with
      | Ok () -> assert false
      | Error e -> Format.printf "%a@." Error_monad.pp_trace e) ;
      [%expect {| Fin.Fun.make: invalid values |}]
  end)

let%test_module "Parallel_pair" =
  (module struct
    let%expect_test "coequalize" =
      let open Error_monad in
      run
        (let* f1 = Fun.make ~values:[| 0; 1; 0; 2 |] ~codom:(Fin 3) in
         let* f2 = Fun.make ~values:[| 0; 0; 1; 2 |] ~codom:(Fin 3) in
         let* p = Parallel_pair.make f1 f2 in
         let* (coeq, coeq_object) = Parallel_pair.coequalize p in
         Format.printf "%a@." Fun.pp_full coeq ;
         Format.printf "%a@." pp_fin coeq_object ;
         return ()) ;
      [%expect {|
          [0,0,1]: Fin(3) => Fin(2)
          Fin(2) |}]
  end)
