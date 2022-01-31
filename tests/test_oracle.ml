open Boltzmann.Oracle

let checkf tolerance = Alcotest.(check (float tolerance))

let checkfa tolerance s a b = Alcotest.(check (array (float tolerance))) s a b

let foi = float_of_int

let iteration grammar z epsilon2 =
  let len = Array.length grammar.Grammar.rules in
  let init_values = init len z in
  match Naive.iteration_simple grammar init_values epsilon2 with
  | Diverge ->
    Alcotest.fail "diverge"
  | Val v ->
    v

(** {2 tests for simple evaluations} *)

let eval_elem () =
  let oracle = {z= 0.; values= [|0.23|]; derivate_values= [|0.|]} in
  checkf 0. "eval(Ref 0)" 0.23 (Eval.expression oracle (Ref 0));
  let oracle = {z= 0.; values= [|0.8|]; derivate_values= [|0.|]} in
  checkf 1e-12 "eval(Seq)" 5. (Eval.expression oracle (Seq (Ref 0)))

let eval_powers_of_z () =
  let test n z =
    let name = Format.sprintf "eval(z^%d)" n in
    let oracle = {z; values= [|0.|]; derivate_values= [|0.|]} in
    checkf 1e-12 name (z ** foi n) (Eval.expression oracle (Z n))
  in
  test 5 0.8; test 10 0.95; test 3 0.01; test 1 0.4

let eval_products () =
  let prod = Grammar.Product (Z 1, Product (Ref 0, Ref 0)) in
  let oracle = {z= 0.25; values= [|2.|]; derivate_values= [|0.|]} in
  checkf 1e-12 "eval(z*A*A)" 1. (Eval.expression oracle prod);
  let prod = Grammar.Product (Z 1, Product (Ref 1, Ref 2)) in
  let oracle = {z= 0.5; values= [|20.; 3.; 4.|]; derivate_values= [|0.|]} in
  checkf 1e-12 "eval(B*C*z)" 6. (Eval.expression oracle prod);
  let prod = Grammar.Product (Z 1, Product (Ref 1, Seq (Ref 3))) in
  let oracle =
    {z= 0.4; values= [|20.; 1.234; 4.; 0.8|]; derivate_values= [|0.|]}
  in
  checkf 1e-12 "eval(B*z*Seq(D)*1)" 2.468 (Eval.expression oracle prod)

let eval_sums () =
  let sum = Grammar.Union (Z 1, Union (Z 1, Z 1)) in
  let z = 0.34567 in
  let oracle = {z; values= [||]; derivate_values= [|0.|]} in
  checkf 1e-12 "eval(z + z + z)" (3. *. z) (Eval.expression oracle sum);
  let sum = Grammar.Union (Ref 0, Union (Seq (Ref 3), Z 1)) in
  let oracle =
    {z= 0.11; values= [|0.33; 10.; 20.; 0.2|]; derivate_values= [|0.|]}
  in
  checkf 1e-12 "eval(A + Seq(D) + z)" 1.69 (Eval.expression oracle sum);
  let sum = Grammar.Union (Product (Ref 0, Ref 0), Union (Z 0, Z 1)) in
  let oracle = {z= 0.87; values= [|0.7|]; derivate_values= [|0.|]} in
  let expected = (oracle.values.(0) ** 2.) +. 1. +. oracle.z in
  checkf 1e-12 "eval(A^2 + 1 + z)" expected (Eval.expression oracle sum)

let eval_plane_trees () =
  let grammar =
    Grammar.
      { names= [|"T"; "S"|]
      ; rules= [|Product (Z 1, Ref 1); Union (Z 0, Product (Ref 0, Ref 1))|] }
  in
  (* at a random point / context *)
  let z = 0.28 in
  let values = [|2.3; 8.1|] in
  let oracle = {z; values; derivate_values= [|0.; 0.|]} in
  let expected = [|z *. 8.1; 1. +. (2.3 *. 8.1)|] in
  checkfa 1e-12 "eval(plane tree)" expected (Eval.grammar oracle grammar).values;
  (* at the singularity *)
  let z = 0.25 in
  let values = [|0.5; 2.|] in
  let oracle = {z; values; derivate_values= [|0.; 0.|]} in
  let expected = values in
  checkfa 1e-12 "eval(plane tree)@singularity" expected
    (Eval.grammar oracle grammar).values

let simple_evaluation_tests =
  [ ("Evaluate atomic elements", `Quick, eval_elem)
  ; ("Evaluate z^n", `Quick, eval_powers_of_z)
  ; ("Evaluate various products", `Quick, eval_products)
  ; ("Evaluate various sums", `Quick, eval_sums)
  ; ("Evaluate the system for plane trees", `Quick, eval_plane_trees) ]

(** {2 Tests for generating function evaluation} *)

let eval_binary () =
  let grammar =
    Grammar.
      { names= [|"B"|]
      ; rules= [|Union (Z 1, Product (Z 1, Product (Ref 0, Ref 0)))|] }
  in
  let oracle z = (1. -. sqrt (1. -. (4. *. z *. z))) /. (2. *. z) in
  let test z =
    let name = Format.sprintf "binary(%F)" z in
    checkf 5e-9 name (oracle z) (iteration grammar z 1e-9).values.(0)
  in
  test 0.1; test 0.3; test 0.4

(* TODO: test 0.5 *)

let eval_nary () =
  let grammar =
    Grammar.
      { names= [|"T"; "S"|]
      ; rules= [|Product (Z 1, Ref 1); Union (Z 0, Product (Ref 0, Ref 1))|] }
  in
  let oracle z =
    let b z = (1. -. sqrt (1. -. (4. *. z))) /. (2. *. z) in
    [|z *. b z; b z|]
  in
  let test z =
    let name = Format.sprintf "nary(%F)" z in
    checkfa 5e-9 name (oracle z) (iteration grammar z 1e-9).values
  in
  test 0.1; test 0.2

(* TODO: test 0.25 *)

let eval_seq () =
  let grammar =
    Grammar.{names= [|"S"|]; rules= [|Product (Z 1, Seq (Ref 0))|]}
  in
  let oracle z = (1. -. sqrt (1. -. (4. *. z))) /. 2. in
  let test z =
    let name = Format.sprintf "seq2(%F)" z in
    checkf 5e-9 name (oracle z) (iteration grammar z 1e-9).values.(0)
  in
  test 0.1; test 0.2

(* TODO: test 0.25 *)

let eval_shuffle_plus () =
  let grammar =
    Grammar.
      { names= [|"A"; "Ashuffle"; "Aplus"|]
      ; rules=
          [| Union (Ref 1, Ref 2)
           ; Product (Z 1, Seq (Ref 0))
           ; Product (Ref 1, Product (Ref 1, Seq (Ref 1))) |] }
  in
  let oracle z =
    let par z = (1. +. z -. sqrt (((1. +. z) ** 2.) -. (8. *. z))) /. 4. in
    [|par z /. (1. -. par z); par z; par z *. par z /. (1. -. par z)|]
  in
  let test z =
    let name = Format.sprintf "shuffle_plus(%F)" z in
    checkfa 5e-9 name (oracle z) (iteration grammar z 1e-9).values
  in
  test 0.05; test 0.1; test 0.15

(* TODO: test (2. -. sqrt 8.) *)

(* TODO: sp *)
(* TODO: unarybinary *)
(* TODO: unarybinary2 *)

let evaluation_tests =
  [ ("Eval binary(z)", `Quick, eval_binary)
  ; ("Eval nary(z)", `Quick, eval_nary)
  ; ("Eval seq(z)", `Quick, eval_seq)
  ; ("Eval shuffle_plus(z)", `Quick, eval_shuffle_plus) ]

(** {2 Tests for the singularity search} *)

let search grammar =
  let oracle = Naive.make_singular grammar in
  oracle.z

let binary_singularity () =
  let grammar =
    Grammar.
      { names= [|"B"|]
      ; rules= [|Union (Z 1, Product (Z 1, Product (Ref 0, Ref 0)))|] }
  in
  checkf 5e-9 "singularity(binary)" 0.5 (search grammar)

let nary_singularity () =
  let grammar =
    Grammar.
      { names= [|"T"; "S"|]
      ; rules= [|Product (Z 1, Ref 1); Union (Z 0, Product (Ref 0, Ref 1))|] }
  in
  checkf 5e-9 "singularity(nary)" 0.25 (search grammar)

let seq_singularity () =
  let grammar =
    Grammar.{names= [|"S"|]; rules= [|Product (Z 1, Seq (Ref 0))|]}
  in
  checkf 5e-9 "singularity(seq)" 0.25 (search grammar)

let shuffle_plus_singularity () =
  let grammar =
    Grammar.
      { names= [|"A"; "Ashuffle"; "Aplus"|]
      ; rules=
          [| Union (Ref 1, Ref 2)
           ; Product (Z 1, Seq (Ref 0))
           ; Product (Ref 1, Product (Ref 1, Seq (Ref 1))) |] }
  in
  let singularity = 3. -. sqrt 8. in
  checkf 5e-9 "singularity(shuffle_plus)" singularity (search grammar)

(* TODO: sp *)
(* TODO: unarybinary *)
(* TODO: unarybinary2 *)

let singularity_tests =
  [ ("Search singularity for binary.spec", `Quick, binary_singularity)
  ; ("Search singularity for nary.spec", `Quick, nary_singularity)
  ; ("Search singularity for seq.spec", `Quick, seq_singularity)
  ; ( "Search singularity for shuffle_plus.spec"
    , `Quick
    , shuffle_plus_singularity ) ]

(** {2 Tests for the expectation search} *)

let search grammar =
  let oracle = Naive.make_expectation 1000 grammar in
  oracle.z

let binary_expectation () =
  (*    B(z) = (1 - sqrt(1 - 4z^2)) / (2 z)
     z B'(z) = B(z) / (1 - 2 z B(z))
             = B(z) / sqrt(1 - 4 z^2)

     expectation = N => z = 0.5 * sqrt(1 - 1 / N^2) *)
  let grammar =
    Grammar.
      { names= [|"B"|]
      ; rules= [|Union (Z 1, Product (Z 1, Product (Ref 0, Ref 0)))|] }
  in
  let tuned_z = sqrt (1. -. 1. /. (1000. ** 2.)) /. 2. in
  checkf 5e-9 "expectation(binary)" tuned_z (search grammar)

let nary_expectation () =
  (*  T(z) = (1 - sqrt(1 - 4 z)) / 2
     T'(z) = 1 / sqrt(1 - 4 z)

     expectation = N => z = 0.25 * (1 - 1 / (2 N - 1)^2) *)
  let grammar =
    Grammar.
      { names= [|"T"; "S"|]
      ; rules= [|Product (Z 1, Ref 1); Union (Z 0, Product (Ref 0, Ref 1))|] }
  in
  let tuned_z = (1. -. 1. /. (2. *. 1000. -. 1.) ** 2.) /. 4. in
  checkf 5e-9 "expectation(nary)" tuned_z (search grammar)

let seq_expectation () =
  (* same as above *)
  let grammar =
    Grammar.{names= [|"S"|]; rules= [|Product (Z 1, Seq (Ref 0))|]}
  in
  let tuned_z = (1. -. 1. /. (2. *. 1000. -. 1.) ** 2.) /. 4. in
  checkf 5e-9 "expectation(seq)" tuned_z (search grammar)

let shuffle_plus_expectation () =
  (* A(z)    = (1 - z - sqrt((1 - z)^2 - 4 z)) / 2
     z A'(z) = (A + A^2) / (1 - A^2 / z) *)
  let grammar =
    Grammar.
      { names= [|"A"; "Ashuffle"; "Aplus"|]
      ; rules=
          [| Union (Ref 1, Ref 2)
           ; Product (Z 1, Seq (Ref 0))
           ; Product (Ref 1, Product (Ref 1, Seq (Ref 1))) |] }
  in
  (* Solved by dichotomy using sage and a the explicit formulas given above. *)
  let tuned_z = 0.171572814532940 in
  checkf 5e-9 "expectation(shuffle_plus)" tuned_z (search grammar)

let expectation_tests =
  [ ("Search expectation for binary.spec", `Quick, binary_expectation)
  ; ("Search expectation for nary.spec", `Quick, nary_expectation)
  ; ("Search expectation for seq.spec", `Quick, seq_expectation)
  ; ( "Search expectation for shuffle_plus.spec"
    , `Quick
    , shuffle_plus_expectation ) ]

(** {2 All the oracle-related tests} *)

let () =
  eval_plane_trees ();
  Alcotest.run "oracle"
    [ ("simple evaluation", simple_evaluation_tests)
    ; ("function approximation", evaluation_tests)
    ; ("singularity search", singularity_tests)
    ; ("expectation search", expectation_tests) ]
