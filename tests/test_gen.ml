open Arbolib


let fail format =
  Format.kasprintf Alcotest.fail format

let rec pp_tree fmt = function
  | Tree.Leaf (typ, _) -> Format.fprintf fmt "%s" typ
  | Tree.Node (typ, _, children) ->
    let pp_sep fmt () = Format.fprintf fmt ",@," in
    Format.fprintf fmt "%s[%a]"
      typ
      (Format.pp_print_list ~pp_sep (fun fmt t -> pp_tree fmt !t)) children

let check_size size_min size_max expected actual =
  if actual <> expected then
    fail "the size computed by arbogen is wrong: %d <> %d" actual expected
  else if actual < size_min || actual > size_max then
    fail "wrong size: %d not in [%d, %d]" actual size_min size_max

let add_opts =
  let add x y = match x, y with
    | Some x, Some y -> Some (x + y)
    | _ -> None
  in
  List.fold_left add (Some 0)


let generate ?seed:(seed=42424242) grammar ~size_min ~size_max =
  match Gen.generator
          grammar
          false (* self seed *)
          seed
          size_min
          size_max
          1e-9 (* epsilon 1 *)
          0.5  (* epsilon_factor 1 *)
          1e-9 (* epsilon 1 *)
          0.5  (* epsilon_factor 1 *)
          100  (* max_try *)
          0.8  (* ratio_rejected *)
          8    (* max_refine *)
          0.   (* zstart *)
          "ocaml" (* randgen *)
          0    (* verbosity *)
  with
  | Some (tree, size, _) -> tree, size
  | None ->
    let name, _ = List.hd grammar in
    fail "generation of %s failed" name


let valid_binary () =
  let grammar = Grammar.[
      "BinNode", [(1, [Elem "Leaf"]); (1, [Elem "BinNode"; Elem "BinNode"])];
      "Leaf", [epsilon]
    ] in
  let size tree =
    let rec aux typ t = match typ, t with
      | `Bin, Tree.Node ("BinNode", _, [l]) -> 1 + aux `Leaf !l
      | `Bin, Tree.Node ("BinNode", _, [l; r]) -> 1 + aux `Bin !l + aux `Bin !r
      | `Leaf, Tree.Leaf ("Leaf", _) -> 0
      | _ -> fail "not a binary tree: %a" pp_tree tree
    in
    aux `Bin tree
  in
  let size_min = 20 in
  let size_max = 30 in
  let tree, gen_size = generate grammar ~size_min ~size_max in
  check_size size_min size_max gen_size (size tree)

let valid_nary () =
  let grammar = Grammar.[
      "NTree", [(1, [Elem "Seq"])];
      "Seq", [(0, [Elem "Leaf"]); (0, [Elem "NTree"; Elem "Seq"])];
      "Leaf", [epsilon]
    ] in
  let size tree =
    let rec aux typ t = match typ, t with
      | `Node, Tree.Node ("NTree", _, [s]) -> 1 + aux `Seq !s
      | `Seq, Tree.Node ("Seq", _, [s]) -> aux `Leaf !s
      | `Seq, Tree.Node ("Seq", _, [x; xs]) -> aux `Node !x + aux `Seq !xs
      | `Leaf, Tree.Leaf ("Leaf", _) -> 0
      | _ -> fail "not an nary tree: %a" pp_tree tree
    in
    aux `Node tree
  in
  let size_min = 20 in
  let size_max = 30 in
  let tree, gen_size = generate grammar ~size_min ~size_max in
  check_size size_min size_max gen_size (size tree)


let () =
  Alcotest.run "gen" [
    "correctness", [
      "binary trees", `Quick, valid_binary;
      "nary trees", `Quick, valid_nary;
    ]
  ]
