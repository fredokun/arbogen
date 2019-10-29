open Grammar
open Oracles
open Oracles.Types

let checkf tolerance = Alcotest.(check (float tolerance))
let checkfa tolerance = Alcotest.(check (array (float tolerance)))
let foi = float_of_int

let iteration grammar z epsilon2 =
  match Naive.iteration_simple grammar z epsilon2 with
  | Diverge -> Alcotest.fail "diverge"
  | Val v -> v

let mk_grammar rules =
  let names = Array.init (Array.length rules) string_of_int in
  {names; rules}


(** {2 tests for simple evaluations} *)

let eval_elem () =
  let oracle = {z = 0.; values = [|0.23|]} in
  checkf 0. "eval(Elem)" 0.23 (Eval.elem oracle (Elem 0));
  let oracle = {z = 0.; values = [|0.8|]} in
  checkf 1e-12 "eval(Seq)" 5. (Eval.elem oracle (Seq 0))

let eval_empty_product () =
  let oracle = {z = 0.23; values = [||]} in
  checkf 0. "eval(empty_prod)" 1. (Eval.component oracle (0, []))

let eval_powers_of_z () =
  let test n z =
    let name = Format.sprintf "eval(z^%d)" n in
    let oracle = {z; values = [||]} in
    checkf 1e-12 name (z ** foi n) (Eval.component oracle (n, []))
  in
  test 5 0.8;
  test 10 0.95;
  test 3 0.01;
  test 1 0.4

let eval_products () =
  let prod = (1, [Elem 0; Elem 0]) in
  let oracle = {z = 0.25; values = [|2.|]} in
  checkf 1e-12 "eval(z*A*A)" 1. (Eval.component oracle prod);

  let prod = (1, [Elem 1; Elem 2]) in
  let oracle = {z = 0.5; values = [|20.; 3.; 4.|]} in
  checkf 1e-12 "eval(B*C*z)" 6. (Eval.component oracle prod);

  let prod = (1, [Elem 1; Seq 3]) in
  let oracle = {z = 0.4; values = [|20.; 1.234; 4.; 0.8|]} in
  checkf 1e-12 "eval(B*z*Seq(D)*1)" 2.468 (Eval.component oracle prod)

let eval_empty_sum () =
  let oracle = {z = 1.; values = [|1.; 2.; 3.|]} in
  checkf 0. "eval(empty_sum)" 0. (Eval.rule oracle [])

let eval_sums () =
  let sum = [(1, []); (1, []); (1, [])] in
  let z = 0.34567 in
  let oracle = {z; values = [||]} in
  checkf 1e-12 "eval(z + z + z)" (3. *. z) (Eval.rule oracle sum);

  let sum = [(0, [Elem 0]); (0, [Seq 3]); (1, [])] in
  let oracle = {z = 0.11; values = [|0.33; 10.; 20.; 0.2|]} in
  checkf 1e-12 "eval(A + Seq(D) + z)" 1.69 (Eval.rule oracle sum);

  let sum = [(0, [Elem 0; Elem 0]); (0, []); (1, [])] in
  let oracle = {z = 0.87; values = [|0.7|]} in
  let expected = oracle.values.(0) ** 2. +. 1. +. oracle.z in
  checkf 1e-12 "eval(A^2 + 1 + z)" expected (Eval.rule oracle sum)

let eval_plane_trees () =
  let grammar = mk_grammar [|
    [(1, [Elem 1])];                 (* T = Z * S      *)
    [(0, []); (0, [Elem 1; Elem 0])] (* S = 1 + T * S  *)
  |] in
  (* at a random point / context *)
  let z = 0.28 in
  let values = [|2.3; 8.1|] in
  let oracle = {z; values} in
  let expected = [|z *. 8.1; 1. +. 2.3 *. 8.1|] in
  checkfa 1e-12 "eval(plane tree)" expected (Eval.grammar oracle grammar);
  (* at the singularity *)
  let z = 0.25 in
  let values = [|0.5; 2.|] in
  let oracle = {z; values} in
  let expected = values in
  checkfa 1e-12 "eval(plane tree)@singularity" expected (Eval.grammar oracle grammar)

let simple_evaluation_tests = [
  "Evaluate atomic elements", `Quick, eval_elem;
  "Evaluate the empty product", `Quick, eval_empty_product;
  "Evaluate z^n", `Quick, eval_powers_of_z;
  "Evaluate various products", `Quick, eval_products;
  "Evaluate the empty sum", `Quick, eval_empty_sum;
  "Evaluate various sums", `Quick, eval_sums;
  "Evaluate the system for plane trees", `Quick, eval_plane_trees;
]


(** {2 Tests for generating function evaluation} *)

let eval_binary () =
  let grammar = mk_grammar [|
      [(1, []); (1, [Elem 0; Elem 0])]
    |] in
  let oracle z = (1. -. sqrt (1. -. 4. *. z *. z)) /. (2. *. z) in
  let test z =
    let name = Format.sprintf "binary(%F)" z in
    checkf 5e-9 name (oracle z) (iteration grammar z 1e-9).(0)
  in
  test 0.1;
  test 0.3;
  test 0.4
(* TODO: test 0.5 *)

let eval_nary () =
  let grammar = mk_grammar [|
      [(1, [Elem 1])];
      [epsilon; (0, [Elem 0; Elem 1])]
    |] in
  let oracle z =
    let b z = (1. -. sqrt (1. -. 4. *. z)) /. (2. *. z) in
    [|z *. b z; b z|]
  in
  let test z =
    let name = Format.sprintf "nary(%F)" z in
    checkfa 5e-9 name (oracle z) (iteration grammar z 1e-9)
  in
  test 0.1;
  test 0.2
(* TODO: test 0.Z5 *)

let eval_seq () =
  let grammar = mk_grammar [| [(1, [Seq 0])] |] in
  let oracle z = (1. -. sqrt (1. -. 4. *. z)) /. 2. in
  let test z =
    let name = Format.sprintf "seq2(%F)" z in
    checkf 5e-9 name (oracle z) (iteration grammar z 1e-9).(0)
  in
  test 0.1;
  test 0.2
(* TODO: test 0.25 *)

let eval_seq2 () =
  let grammar = mk_grammar [|
      [(1, [Elem 1])];
      [epsilon; (0, [Elem 0; Elem 1])];
    |] in
  let oracle z =
    let b z = (1. -. sqrt (1. -. 4. *. z)) /. (2. *. z) in
    [| z *. b z; b z |]
  in
  let test z =
    let name = Format.sprintf "seq2(%F)" z in
    checkfa 5e-9 name (oracle z) (iteration grammar z 1e-9)
  in
  test 0.1;
  test 0.2
(* TODO: test 0.25 *)

let eval_shuffle_plus () =
  let grammar = mk_grammar [|
      [(0, [Elem 1]); (0, [Elem 2])];
      [(1, [Seq 0])];
      [(0, [Elem 1; Elem 1; Seq 1])]
    |] in
  let oracle z =
    let par z = (1. +. z -. sqrt ((1. +. z) ** 2. -. 8. *. z)) /. 4. in
    [|
      par z /. (1. -. par z);
      par z;
      par z *. par z /. (1. -. par z)
    |]
  in
  let test z =
    let name = Format.sprintf "shuffle_plus(%F)" z in
    checkfa 5e-9 name (oracle z) (iteration grammar z 1e-9)
  in
  test 0.05;
  test 0.1;
  test 0.15
(* TODO: test (2. -. sqrt 8.) *)

(* TODO: sp *)
(* TODO: unarybinary *)
(* TODO: unarybinary2 *)

let evaluation_tests = [
  "Eval binary(z)", `Quick, eval_binary;
  "Eval nary(z)", `Quick, eval_nary;
  "Eval seq(z)", `Quick, eval_seq;
  "Eval seq2(z)", `Quick, eval_seq2;
  "Eval shuffle_plus(z)", `Quick, eval_shuffle_plus;
]


(** {2 Tests for the singularity search} *)

let eps = 1e-9

let search grammar =
  let config = Naive.{epsilon1 = eps; epsilon2 = eps; zmin = 0.; zmax = 1.; zstart = 0.} in
  let oracle = Naive.make config grammar in
  oracle.z

let binary_singularity () =
  let grammar = mk_grammar [|
      [(1, []); (1, [Elem 0; Elem 0])];
    |] in
  checkf 5e-9 "singularity(binary)" 0.5 (search grammar)

let nary_singularity () =
  let grammar = mk_grammar [|
      [(1, [Elem 1])];
      [epsilon; (0, [Elem 0; Elem 1])];
    |] in
  checkf 5e-9 "singularity(nary)" 0.25 (search grammar)

let seq_singularity () =
  let grammar = mk_grammar [| [(1, [Seq 0])] |] in
  checkf 5e-9 "singularity(seq)" 0.25 (search grammar)

let seq2_singularity () =
  let grammar = mk_grammar [|
      [(1, [Elem 1])];
      [epsilon; (0, [Elem 0; Elem 1])];
    |] in
  checkf 5e-9 "singularity(seq2)" 0.25 (search grammar)

let shuffle_plus_singularity () =
  let grammar = mk_grammar [|
      [(0, [Elem 1]); (0, [Elem 2])];
      [(1, [Seq 0])];
      [(0, [Elem 1; Elem 1; Seq 1])]
    |] in
  let singularity = 3. -. sqrt 8. in
  checkf 5e-9 "singularity(shuffle_plus)" singularity (search grammar)

(* TODO: sp *)
(* TODO: unarybinary *)
(* TODO: unarybinary2 *)

let singularity_tests = [
  "Search singularity for binary.spec", `Quick, binary_singularity;
  "Search singularity for nary.spec", `Quick, nary_singularity;
  "Search singularity for seq.spec", `Quick, seq_singularity;
  "Search singularity for seq2.spec", `Quick, seq2_singularity;
  "Search singularity for shuffle_plus.spec", `Quick, shuffle_plus_singularity;
]


(** {2 All the oracle-related tests} *)

let () =
  Alcotest.run "oracle" [
    "simple evaluation", simple_evaluation_tests;
    "function approximation", evaluation_tests;
    "singularity search", singularity_tests
  ]
