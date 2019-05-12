open Arbolib
open Grammar

let checkf tolerance = Alcotest.(check (float tolerance))
let checkfa tolerance = Alcotest.(check (array (float tolerance)))
let foi = float_of_int

let mk_grammar rules =
  let names = Array.init (Array.length rules) string_of_int in
  {names; rules}


(** {2 tests for system evaluation} *)

let eval_elem () =
  checkf 0. "eval(Elem)" 0.23 (eval_elem [|0.23|] (Elem 0));
  checkf 1e-12 "eval(Seq)" 5. (eval_elem [|0.8|] (Seq 0))

let eval_empty_product () =
  checkf 0. "eval(empty_prod)" 1. (eval_component 0.23 [||] (0, []))

let eval_powers_of_z () =
  let test n z =
    let name = Format.sprintf "eval(z^%d)" n in
    checkf 1e-12 name (z ** foi n) (eval_component z [||] (n, []))
  in
  test 5 0.8;
  test 10 0.95;
  test 3 0.01;
  test 1 0.4

let eval_products () =
  let prod = (1, [Elem 0; Elem 0]) in
  let z = 0.25 in
  let context = [|2.|] in
  checkf 1e-12 "eval(z*A*A)" 1. (eval_component z context prod);

  let prod = (1, [Elem 1; Elem 2]) in
  let z = 0.5 in
  let context = [|20.; 3.; 4.|] in
  checkf 1e-12 "eval(B*C*z)" 6. (eval_component z context prod);

  let prod = (1, [Elem 1; Seq 3]) in
  let z = 0.4 in
  let context = [|20.; 1.234; 4.; 0.8|] in
  checkf 1e-12 "eval(B*z*Seq(D)*1)" 2.468 (eval_component z context prod)

let eval_empty_sum () =
  checkf 0. "eval(empty_sum)" 0. (eval_rule 1. [|1.; 2.; 3.|] [])

let eval_sums () =
  let sum = [(1, []); (1, []); (1, [])] in
  let z = 0.34567 in
  checkf 1e-12 "eval(z + z + z)" (3. *. z) (eval_rule z [||] sum);

  let sum = [(0, [Elem 0]); (0, [Seq 3]); (1, [])] in
  let z = 0.11 in
  let context = [|0.33; 10.; 20.; 0.2|] in
  checkf 1e-12 "eval(A + Seq(D) + z)" 1.69 (eval_rule z context sum);

  let sum = [(0, [Elem 0; Elem 0]); (0, []); (1, [])] in
  let z = 0.87 in
  let context = [|0.7|] in
  checkf 1e-12 "eval(A^2 + 1 + z)" (0.7 ** 2. +. 1. +. z) (eval_rule z context sum)

let eval_plane_trees () =
  let grammar = mk_grammar [|
    [(1, [Elem 1])];                 (* T = Z * S      *)
    [(0, []); (0, [Elem 1; Elem 0])] (* S = 1 + T * S  *)
  |] in
  (* at a random point / context *)
  let z = 0.28 in
  let context = [|2.3; 8.1|] in
  let expected = [|z *. 8.1; 1. +. 2.3 *. 8.1|] in
  checkfa 1e-12 "eval(plane tree)" expected (eval z context grammar);
  (* at the singularity *)
  let z = 0.25 in
  let context = [|0.5; 2.|] in
  let expected = context in
  checkfa 1e-12 "eval(plane tree)@singularity" expected (eval z context grammar)

let evaluation_tests = [
  "Evaluate atomic elements", `Quick, eval_elem;
  "Evaluate the empty product", `Quick, eval_empty_product;
  "Evaluate z^n", `Quick, eval_powers_of_z;
  "Evaluate various products", `Quick, eval_products;
  "Evaluate the empty sum", `Quick, eval_empty_sum;
  "Evaluate various sums", `Quick, eval_sums;
  "Evaluate the system for plane trees", `Quick, eval_plane_trees;
]

let () =
  Alcotest.run "combsys" [
    "evaluation", evaluation_tests;
  ]
