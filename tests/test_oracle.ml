open Arbolib
open Grammar
open OracleSimple

let checkf tolerance = Alcotest.(check (float tolerance))
let checkfa tolerance = Alcotest.(check (array (float tolerance)))

let iteration grammar z epsilon2 =
  match iterationSimple grammar z epsilon2 with
  | Diverge -> Alcotest.fail "diverge"
  | Val v -> v

let mk_grammar rules =
  let names = Array.init (Array.length rules) string_of_int in
  {names; rules}

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
  let config = {epsilon1 = eps; epsilon2 = eps; zmin = 0.; zmax = 1.; zstart = 0.} in
  let zmin, _, _ = searchSingularity config grammar in
  zmin

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
    "function approximation", evaluation_tests;
    "singularity search", singularity_tests
  ]
