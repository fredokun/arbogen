open Arbolib
open CombSys

let checkf tolerance = Alcotest.(check (float tolerance))
let checkfa tolerance = Alcotest.(check (array (float tolerance)))
let foi = float_of_int

let combsys = Alcotest.testable CombSys.pp CombSys.eq


(** {2 tests for system evaluation} *)

let eval_combnode () =
  checkf 0. "eval(z)" 0.42 (eval_combnode 0.42 [|0.|] Z);
  checkf 0. "eval(Ref)" 0.23 (eval_combnode 0.6 [|0.23|] (Refe 0));
  checkf 1e-12 "eval(Seq)" 5. (eval_combnode 0.3 [|0.8|] (Seq 0))

let eval_empty_product () =
  checkf 0. "eval(empty_prod)" 1. (eval_combprod 0.23 [||] [])

let eval_powers_of_z () =
  let rec powz = function
    | 0 -> []
    | n -> Z :: powz (n - 1)
  in
  let test n z =
    let name = Format.sprintf "eval(z^%d)" n in
    checkf 1e-12 name (z ** foi n) (eval_combprod z [||] (powz n))
  in
  test 5 0.8;
  test 10 0.95;
  test 3 0.01;
  test 1 0.4

let eval_products () =
  let prod = [Z; Refe 0; Refe 0] in
  let z = 0.25 in
  let context = [|2.|] in
  checkf 1e-12 "eval(z*A*A)" 1. (eval_combprod z context prod);

  let prod = [Refe 1; Refe 2; Z] in
  let z = 0.5 in
  let context = [|20.; 3.; 4.|] in
  checkf 1e-12 "eval(B*C*z)" 6. (eval_combprod z context prod);

  let prod = [Refe 1; Z; Seq 3] in
  let z = 0.4 in
  let context = [|20.; 1.234; 4.; 0.8|] in
  checkf 1e-12 "eval(B*z*Seq(D)*1)" 2.468 (eval_combprod z context prod)

let eval_empty_sum () =
  checkf 0. "eval(empty_sum)" 0. (eval_eq 1. [|1.; 2.; 3.|] [])

let eval_sums () =
  let sum = [[Z]; [Z]; [Z]] in
  let z = 0.34567 in
  checkf 1e-12 "eval(z + z + z)" (3. *. z) (eval_eq z [||] sum);

  let sum = [[Refe 0]; [Seq 3]; [Z]] in
  let z = 0.11 in
  let context = [|0.33; 10.; 20.; 0.2|] in
  checkf 1e-12 "eval(A + Seq(D) + z)" 1.69 (eval_eq z context sum);

  let sum = [[Refe 0; Refe 0]; []; [Z]] in
  let z = 0.87 in
  let context = [|0.7|] in
  checkf 1e-12 "eval(A^2 + 1 + z)" (0.7 ** 2. +. 1. +. z) (eval_eq z context sum)

let eval_plane_trees () =
  let sys = [|
    [[Z; Refe 1]];           (* T = Z * S      *)
    [[]; [Refe 1; Refe 0]]   (* S = 1 + T * S  *)
  |] in
  (* at a random point / context *)
  let z = 0.28 in
  let context = [|2.3; 8.1|] in
  let expected = [|z *. 8.1; 1. +. 2.3 *. 8.1|] in
  checkfa 1e-12 "eval(plane tree)" expected (evaluation sys z context);
  (* at the singularity *)
  let z = 0.25 in
  let context = [|0.5; 2.|] in
  let expected = context in
  checkfa 1e-12 "eval(plane tree)@singularity" expected (evaluation sys z context)

let evaluation_tests = [
  "Evaluate combinatorial nodes", `Quick, eval_combnode;
  "Evaluate the empty product", `Quick, eval_empty_product;
  "Evaluate z^n", `Quick, eval_powers_of_z;
  "Evaluate various products", `Quick, eval_products;
  "Evaluate the empty sum", `Quick, eval_empty_sum;
  "Evaluate various sums", `Quick, eval_sums;
  "Evaluate the system for plane trees", `Quick, eval_plane_trees;
]


(** {2 tests for the conversion from grammars to combinatorial systems} *)

let convert_binary () =
  let grammar = Grammar.[
      "BinNode", [
        (1, [Elem "Leaf"]);
        (1, [Elem "BinNode"; Elem "BinNode"])
      ];
      "Leaf", [epsilon]
    ] in
  let expected = [|
    [[Z; Refe 1]; [Z; Refe 0; Refe 0]];
    [[]]
  |] in
  Alcotest.check combsys "convert binary spec" expected (combsys_of_grammar grammar)

let convert_nary () =
  let grammar = Grammar.[
      "NTree", [(1, [Elem "Seq"])];
      "Seq", [
        (0, [Elem "Leaf"]);
        (0, [Elem "NTree"; Elem "Seq"])
      ];
      "Leaf", [epsilon]
    ] in
  let expected = [|
    [[Z; Refe 1]];
    [[Refe 2]; [Refe 0; Refe 1]];
    [[]]
  |] in
  Alcotest.check combsys "convert nary spec" expected (combsys_of_grammar grammar)

let convert_seq () =
  let grammar = Grammar.[
      "Node", [(1, [Seq "Node"])]
    ] in
  let expected = [|
    [[Z; Seq 0]]
  |] in
  Alcotest.check combsys "convert seq spec" expected (combsys_of_grammar grammar)

let convert_seq2 () =
  let grammar = Grammar.[
      "Node", [(1, [Elem "Seq"])];
      "Seq", [
        (0, []);
        (0, [Elem "Node"; Elem "Seq"])
      ];
    ] in
  let expected = [|
    [[Z; Refe 1]];
    [[]; [Refe 0; Refe 1]]
  |] in
  Alcotest.check combsys "convert seq2 spec" expected (combsys_of_grammar grammar)

let convert_shuffle_plus () =
  let grammar = Grammar.[
      "A", [
        (0, [Elem "Ashuffle"]);
        (0, [Elem "Aplus"])
      ];
      "Ashuffle", [(1, [Seq "A"])];
      "Aplus", [(0, [Elem "Ashuffle"; Elem "Ashuffle"; Seq "Ashuffle"])];
    ] in
  let expected = [|
    [[Refe 1]; [Refe 2]];
    [[Z; Seq 0]];
    [[Refe 1; Refe 1; Seq 1]]
  |] in
  Alcotest.check combsys "convert shuffle_plus spec" expected (combsys_of_grammar grammar)

let convert_sp () =
  let grammar = Grammar.[
      "T", [
        (1, []);
        (1, [Elem "T"]);
        (1, [Elem "T"; Elem "T"; Elem "T"])]
    ] in
  let expected = [|
    [[Z]; [Z; Refe 0]; [Z; Refe 0; Refe 0; Refe 0]]
  |] in
  Alcotest.check combsys "convert sp spec" expected (combsys_of_grammar grammar)

let convert_unarybinary () =
  let grammar = Grammar.[
      "UBTree", [
        (1, []);
        (1, [Elem "UBTree"]);
        (1, [Elem "UBTree"; Elem "UBTree"])]
    ] in
  let expected = [|
    [[Z]; [Z; Refe 0]; [Z; Refe 0; Refe 0]]
  |] in
  Alcotest.check combsys "convert unarybinary spec" expected (combsys_of_grammar grammar)

let convert_unarybinary2 () =
  let grammar = Grammar.[
      "UBTree", [
        (0, [Elem "UBLeaf"]);
        (0, [Elem "Unary"]);
        (0, [Elem "Binary"])
      ];
      "Unary", [(1, [Elem "UBTree"])];
      "Binary", [(1, [Elem "UBTree"; Elem "UBTree"])];
      "UBLeaf", [(1, [])]
    ] in
  let expected = [|
    [[Refe 1]; [Refe 2]; [Refe 3]];
    [[Z; Refe 0]];
    [[Z; Refe 0; Refe 0]];
    [[Z]]
  |] in
  Alcotest.check combsys "convert unarybinary spec" expected (combsys_of_grammar grammar)

let conversion_tests = [
  "Convert binary", `Quick, convert_binary;
  "Convert nary", `Quick, convert_nary;
  "Convert seq", `Quick, convert_seq;
  "Convert seq2", `Quick, convert_seq2;
  "Convert shuffle_plus spec", `Quick, convert_shuffle_plus;
  "Convert sp", `Quick, convert_sp;
  "Convert unarybinary", `Quick, convert_unarybinary;
  "Convert unarybinary2", `Quick, convert_unarybinary2;
]

(** {2 All the test for the CombSys module} *)

let () =
  Alcotest.run "combsys" [
    "evaluation", evaluation_tests;
    "conversion", conversion_tests
  ]
