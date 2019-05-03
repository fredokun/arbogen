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

(** {2 Correctness tests} *)

exception Invalid

let valid_binary () =
  let size_min, size_max = 20, 30 in
  let grammar = Grammar.["Node", [(0, []); (1, [Elem "Node"; Elem "Node"])]] in
  let rec size = function
    | Tree.Node ("Node", _, [l; r]) -> 1 + size !l + size !r
    | Tree.Leaf ("Node", _) -> 0
    | _ -> raise Invalid
  in
  let tree, gen_size = generate grammar ~size_min ~size_max in
  try check_size size_min size_max gen_size (size tree)
  with Invalid -> fail "not a binary tree: %a" pp_tree tree

let valid_nary () =
  let size_min, size_max = 20, 30 in
  let grammar = Grammar.[
    "T", [(1, [Elem "S"])];
    "S", [(0, []); (0, [Elem "T"; Elem "S"])];
  ] in
  let rec size = function
    | Tree.Node ("T", _, [s]) -> 1 + size !s
    | Tree.Leaf ("S", _) -> 0
    | Tree.Node ("S", _, [x; xs]) -> size !x + size !xs
    | _ -> raise Invalid
  in
  let tree, gen_size = generate grammar ~size_min ~size_max in
  try check_size size_min size_max gen_size (size tree)
  with Invalid -> fail "not an nary tree: %a" pp_tree tree

let valid_nary_bis () =
  let size_min, size_max = 20, 30 in
  let grammar = Grammar.["T", [(1, [Seq "T"])]] in
  let rec size = function
    | Tree.Leaf ("T", _) -> 1
    | Tree.Node ("T", _, children) -> List.fold_left (fun acc t -> acc + size !t) 1 children
    | _ -> raise Invalid
  in
  let tree, gen_size = generate grammar ~size_min ~size_max in
  try check_size size_min size_max gen_size (size tree)
  with Invalid -> fail "not an nary tree (bis): %a" pp_tree tree

let correctness = [
  "binary trees", `Quick, valid_binary;
  "nary trees", `Quick, valid_nary;
  "nary trees (bis)", `Quick, valid_nary_bis;
]


(** {2 Uniformity tests} *)

let incr repr store (tree, size) =
  let r = repr tree in
  match Hashtbl.find_opt store.(size) r with
  | None -> Hashtbl.add store.(size) r 1
  | Some nb -> Hashtbl.replace store.(size) r (nb + 1)

let unif_binary () =
  Random.self_init ();
  let grammar = Grammar.["Node", [(0, []); (1, [Elem "Node"; Elem "Node"])]] in
  let store = Array.init 6 (fun _ -> Hashtbl.create 17) in
  let rec repr = function
    | Tree.Leaf ("Node", _) -> ""
    | Tree.Node ("Node", _, [l; r]) -> "(" ^ repr !l ^ ")" ^ repr !r
    | _ -> invalid_arg "repr"
  in
  let nb_iterations = 1000 in (* 1000 is not enough *)
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

let uniformity = [
  "binary trees", `Slow, unif_binary;
]


(** {2 All the tests} *)

let () =
  Alcotest.run "gen" [
    "correctness", correctness;
    "uniformity", uniformity;
  ]
