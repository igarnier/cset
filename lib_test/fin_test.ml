open Cset
open Fin

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
