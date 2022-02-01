let fail format = Format.kasprintf (fun s -> Alcotest.fail s) format

(* XXX. *)
let rec pp_tree fmt tree =
  let (Tree.Node (typ, children)) = tree in
  let pp_sep fmt () = Format.fprintf fmt ",@," in
  Format.fprintf fmt "%s[%a]" typ
    (Format.pp_print_list ~pp_sep pp_tree)
    children

let check_size size_min size_max expected actual =
  if actual <> expected then
    fail "the size computed by arbogen is wrong: %d <> %d" actual expected
  else if actual < size_min || actual > size_max then
    fail "wrong size: %d not in [%d, %d]" actual size_min size_max

let generate_from_wg wg ~size_min ~size_max =
  let module Rng = Randtools.OcamlRandom in
  let max_try = 1_000_000 in
  match Boltzmann.search_seed (module Rng) ~size_min ~size_max ~max_try wg with
  | Some (_, state) ->
    Rng.set_state state;
    Boltzmann.free_gen (module Rng) wg wg.names.(0)
  | None ->
    assert false

let generate ?(seed = 42424242) grammar size_min size_max =
  let expectation = (size_min + size_max) / 2 in
  let oracle = Boltzmann.Oracle.Naive.make_expectation expectation grammar in
  Randtools.OcamlRandom.init seed;
  let wg = Boltzmann.WeightedGrammar.of_grammar oracle grammar in
  generate_from_wg wg ~size_min ~size_max

(** {2 Correctness tests} *)

exception Invalid

let valid_binary () =
  let size_min, size_max = (20, 30) in
  let grammar =
    Grammar.{names= [|"B"|]; rules= [|Union [Z 0; Product [Z 1; Ref 0; Ref 0]]|]}
  in
  let rec size = function
    | Tree.Node ("B", [l; r]) ->
      1 + size l + size r
    | Tree.Node ("B", []) ->
      0
    | _ ->
      raise Invalid
  in
  let tree, gen_size = generate grammar size_min size_max in
  try check_size size_min size_max gen_size (size tree)
  with Invalid -> fail "not a binary tree: %a" pp_tree tree

let valid_nary () =
  let size_min, size_max = (20, 30) in
  let grammar =
    Grammar.
      { names= [|"T"; "S"|]
      ; rules= [|Product [Z 1; Ref 1]; Union [Z 0; Product [Ref 0; Ref 1]]|] }
  in
  let rec size = function
    | Tree.Node ("T", [s]) ->
      1 + size s
    | Tree.Node ("S", []) ->
      0
    | Tree.Node ("S", [x; xs]) ->
      size x + size xs
    | _ ->
      raise Invalid
  in
  let tree, gen_size = generate grammar size_min size_max in
  try check_size size_min size_max gen_size (size tree)
  with Invalid -> fail "not an nary tree: %a" pp_tree tree

let valid_nary_bis () =
  let size_min, size_max = (20, 30) in
  let grammar =
    Grammar.{names= [|"T"|]; rules= [|Product [Z 1; Seq (Ref 0)]|]}
  in
  let rec size = function
    | Tree.Node ("T", []) ->
      1
    | Tree.Node ("T", children) ->
      List.fold_left (fun acc t -> acc + size t) 1 children
    | _ ->
      raise Invalid
  in
  let tree, gen_size = generate grammar size_min size_max in
  try check_size size_min size_max gen_size (size tree)
  with Invalid -> fail "not an nary tree (bis): %a" pp_tree tree

let valid_motzkin () =
  let size_min, size_max = (40, 50) in
  let grammar =
    Grammar.
      { names= [|"M"|]
      ; rules= [|Union [Z 0; Product [Z 1; Ref 0]; Product [Z 1; Ref 0; Ref 0]]|]
      }
  in
  let rec size = function
    | Tree.Node ("M", []) ->
      0
    | Tree.Node ("M", [t]) ->
      1 + size t
    | Tree.Node ("M", [l; r]) ->
      1 + size l + size r
    | _ ->
      raise Invalid
  in
  let tree, gen_size = generate grammar size_min size_max in
  try check_size size_min size_max gen_size (size tree)
  with Invalid -> fail "not a Motzkin tree: %a" pp_tree tree

let valid_shuffle_plus () =
  let size_min, size_max = (10, 100) in
  let grammar =
    Grammar.
      { names= [|"A"; "Ashuffle"; "Aplus"|]
      ; rules=
          [| Union [Ref 1; Ref 2]
           ; Product [Z 1; Seq (Ref 0)]
           ; Product [Ref 1; Ref 1; Seq (Ref 1)] |] }
  in
  let get_type = function
    | Tree.Node ("Aplus", _) ->
      `plus
    | Tree.Node ("Ashuffle", _) ->
      `shuffle
    | _ ->
      raise Invalid
  in
  let sum size_fun = List.fold_left (fun acc t -> acc + size_fun t) 0 in
  let rec size typ tree =
    match (typ, tree) with
    | `A, Tree.Node ("A", [t]) ->
      size (get_type t) t
    | `plus, Tree.Node ("Aplus", children) ->
      if List.compare_length_with children 2 < 0 then raise Invalid;
      sum (size `shuffle) children
    | `shuffle, Tree.Node ("Ashuffle", []) ->
      1
    | `shuffle, Tree.Node ("Ashuffle", children) ->
      1 + sum (size `A) children
    | _ ->
      raise Invalid
  in
  let tree, gen_size = generate ~seed:1234512345 grammar size_min size_max in
  try check_size size_min size_max gen_size (size `A tree)
  with Invalid -> fail "not a shuffle+ tree: %a" pp_tree tree

let correctness =
  [ ("binary trees", `Quick, valid_binary)
  ; ("nary trees", `Quick, valid_nary)
  ; ("nary trees (bis)", `Quick, valid_nary_bis)
  ; ("Motzkin trees", `Quick, valid_motzkin)
  ; ("shuffle+ trees", `Quick, valid_shuffle_plus) ]

(** {2 Statistical tests} *)

(** Chi square test of adequation. *)
let chi_square degree ~expected ~actual =
  Format.eprintf "expected = ";
  Array.iter (Format.eprintf "%F ") expected;
  Format.eprintf "\n";
  let chi2 = [|0.; 3.84; 5.99; 7.81; 9.49; 11.07|] in
  if
    degree >= Array.length chi2
    || Array.length expected <> degree + 1
    || Array.length actual <> degree + 1
  then invalid_arg "chi_square";
  let _, test =
    Array.fold_left
      (fun (i, t) ni ->
        let actual = float_of_int ni in
        (i + 1, t +. (((expected.(i) -. actual) ** 2.) /. expected.(i))) )
      (0, 0.) actual
  in
  Format.eprintf "(%F <= %F) ?@\n" test chi2.(degree);
  test <= chi2.(degree)

module Binary = struct
  let catalan = [|1; 1; 2; 5; 14; 42|]

  let oracle, grammar =
    let g =
      Grammar.
        {names= [|"B"|]; rules= [|Union [Z 0; Product [Z 1; Ref 0; Ref 0]]|]}
    in
    let oracle = Boltzmann.Oracle.Naive.make_expectation 3 g in
    (oracle, Boltzmann.WeightedGrammar.of_grammar oracle g)

  let rank =
    let convolution n k =
      let rec sum acc i =
        if i > k then acc
        else sum (acc + (catalan.(i) * catalan.(n - 1 - i))) (i + 1)
      in
      sum 0 0
    in
    let rec size_and_rank : string Tree.t -> int * int = function
      | Node ("B", []) ->
        (0, 0)
      | Node ("B", [l; r]) ->
        let s1, r1 = size_and_rank l in
        let s2, r2 = size_and_rank r in
        let size = s1 + s2 + 1 in
        let rank = convolution size (s1 - 1) + (r1 * catalan.(s2)) + r2 in
        (size, rank)
      | _ ->
        assert false
    in
    fun tree -> snd (size_and_rank tree)

  (** Adequation with the Boltzmann distribution. *)
  let boltzmann_dist () =
    let store = Array.make 6 0 in
    let nb_iterations = 50000 in
    for _ = 1 to nb_iterations do
      let _, size = generate_from_wg grammar ~size_min:0 ~size_max:5 in
      store.(size) <- store.(size) + 1
    done;
    (* Chi-square test *)
    Format.eprintf "z = %F@." oracle.z;
    let expected =
      let foi = float_of_int in
      let arr =
        Array.init 6 (fun i -> foi catalan.(i) *. (oracle.z ** foi i))
      in
      let total_weight = Array.fold_left ( +. ) 0. arr in
      Array.iteri
        (fun i x -> arr.(i) <- x /. total_weight *. foi nb_iterations)
        arr;
      arr
    in
    Alcotest.(check bool)
      "distribution(size(binary))" true
      (chi_square 5 ~expected ~actual:store)

  (** Adequation with the uniform distribution at fixed size. *)
  let unif_dist () =
    let len = catalan.(3) in
    let store = Array.make len 0 in
    let nb_iterations = 5000 in
    for _ = 1 to nb_iterations do
      let tree, _ = generate_from_wg grammar ~size_min:3 ~size_max:3 in
      let r = rank tree in
      store.(r) <- store.(r) + 1
    done;
    (* Chi-square test *)
    let expected =
      Array.make len (float_of_int nb_iterations /. float_of_int len)
    in
    Alcotest.(check bool)
      "distribution(size(binary))" true
      (chi_square (len - 1) ~expected ~actual:store)
end

let statistical_tests =
  [ ("binary / boltz", `Quick, Binary.boltzmann_dist)
  ; ("binary / unif", `Quick, Binary.unif_dist) ]

(** {2 All the tests} *)

let () =
  Random.self_init ();
  Alcotest.run "gen_expect"
    [("correctness", correctness); ("statistical_tests", statistical_tests)]
