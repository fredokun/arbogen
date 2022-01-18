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

let generate ?(seed = 42424242) grammar ~size_min ~size_max =
  let oracle = Boltzmann.Oracle.Naive.make grammar in
  let module Rng = Randtools.OcamlRandom in
  Rng.init seed;
  match
    Boltzmann.generator grammar oracle
      (module Rng)
      ~size_min ~size_max ~max_try:1000
  with
  | Some (tree, size) ->
    (tree, size)
  | None ->
    let name = grammar.Grammar.names.(0) in
    fail "generation of %s failed" name

(** {2 Correctness tests} *)

exception Invalid

let valid_binary () =
  let size_min, size_max = (20, 30) in
  let grammar =
    Grammar.
      { names= [|"B"|]
      ; rules= [|Union (Z 0, Product (Z 1, Product (Ref 0, Ref 0)))|] }
  in
  let rec size = function
    | Tree.Node ("B", [l; r]) ->
      1 + size l + size r
    | Tree.Node ("B", []) ->
      0
    | _ ->
      raise Invalid
  in
  let tree, gen_size = generate grammar ~size_min ~size_max in
  try check_size size_min size_max gen_size (size tree)
  with Invalid -> fail "not a binary tree: %a" pp_tree tree

let valid_nary () =
  let size_min, size_max = (20, 30) in
  let grammar =
    Grammar.
      { names= [|"T"; "S"|]
      ; rules= [|Product (Z 1, Ref 1); Union (Z 0, Product (Ref 0, Ref 1))|] }
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
  let tree, gen_size = generate grammar ~size_min ~size_max in
  try check_size size_min size_max gen_size (size tree)
  with Invalid -> fail "not an nary tree: %a" pp_tree tree

let valid_nary_bis () =
  let size_min, size_max = (20, 30) in
  let grammar =
    Grammar.{names= [|"T"|]; rules= [|Product (Z 1, Seq (Ref 0))|]}
  in
  let rec size = function
    | Tree.Node ("T", []) ->
      1
    | Tree.Node ("T", children) ->
      List.fold_left (fun acc t -> acc + size t) 1 children
    | _ ->
      raise Invalid
  in
  let tree, gen_size = generate grammar ~size_min ~size_max in
  try check_size size_min size_max gen_size (size tree)
  with Invalid -> fail "not an nary tree (bis): %a" pp_tree tree

let valid_motzkin () =
  let size_min, size_max = (40, 50) in
  let grammar =
    Grammar.
      { names= [|"M"|]
      ; rules=
          [| Union
               ( Z 0
               , Union
                   (Product (Z 1, Ref 0), Product (Z 1, Product (Ref 0, Ref 0)))
               ) |] }
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
  let tree, gen_size = generate grammar ~size_min ~size_max in
  try check_size size_min size_max gen_size (size tree)
  with Invalid -> fail "not a Motzkin tree: %a" pp_tree tree

let valid_shuffle_plus () =
  let size_min, size_max = (10, 100) in
  let grammar =
    Grammar.
      { names= [|"A"; "Ashuffle"; "Aplus"|]
      ; rules=
          [| Union (Ref 1, Ref 2)
           ; Product (Z 1, Seq (Ref 0))
           ; Product (Ref 1, Product (Ref 1, Seq (Ref 1))) |] }
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
  let tree, gen_size = generate ~seed:1234512345 grammar ~size_min ~size_max in
  try check_size size_min size_max gen_size (size `A tree)
  with Invalid -> fail "not a shuffle+ tree: %a" pp_tree tree

let correctness =
  [ ("binary trees", `Quick, valid_binary)
  ; ("nary trees", `Quick, valid_nary)
  ; ("nary trees (bis)", `Quick, valid_nary_bis)
  ; ("Motzkin trees", `Quick, valid_motzkin)
  ; ("shuffle+ trees", `Quick, valid_shuffle_plus) ]

(** {2 Uniformity tests} *)

let incr repr store (tree, size) =
  let r = repr tree in
  match Hashtbl.find_opt store.(size) r with
  | None ->
    Hashtbl.add store.(size) r 1
  | Some nb ->
    Hashtbl.replace store.(size) r (nb + 1)

let unif_binary () =
  Random.self_init ();
  let grammar =
    Grammar.
      { names= [|"B"|]
      ; rules= [|Union (Z 0, Product (Z 1, Product (Ref 0, Ref 0)))|] }
  in
  let store = Array.init 6 (fun _ -> Hashtbl.create 17) in
  let rec repr = function
    | Tree.Node ("B", []) ->
      ""
    | Tree.Node ("B", [l; r]) ->
      "(" ^ repr l ^ ")" ^ repr r
    | _ ->
      invalid_arg "repr"
  in
  let nb_iterations = 1000 in
  (* 1000 is not enough *)
  for _ = 0 to nb_iterations - 1 do
    generate grammar ~seed:(Random.bits ()) ~size_min:0 ~size_max:5
    |> incr repr store
  done;
  let check size _ nb =
    let size = float_of_int size in
    let probability = float_of_int nb /. float_of_int nb_iterations in
    let expected = (0.25 ** size) /. 2. in
    (* XXX. crappy tolerance *)
    Alcotest.(check (float 0.2)) "distribution(binary)" probability expected
  in
  Array.iteri (fun size tbl -> Hashtbl.iter (check size) tbl) store

let uniformity = [("binary trees", `Slow, unif_binary)]

(** {2 All the tests} *)

let () =
  Alcotest.run "gen" [("correctness", correctness); ("uniformity", uniformity)]
