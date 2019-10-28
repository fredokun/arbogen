type symbol =
  | Z of int
  | Elem of string
  | Seq of string
type product = symbol list
type rule = string * product list
type rules = rule list


(** {2 Grammar completion} *)

module Sset = Set.Make(String)

let symbol_names set = function
  | Z _ -> set
  | Elem name -> Sset.add name set
  | Seq name -> Sset.add name set

let product_names = List.fold_left symbol_names

let rule_names set (_, terms) =
  List.fold_left product_names set terms

let names = List.fold_left rule_names Sset.empty

(** Transform every undefined non-terminal into an leaf *)
let completion grammar =
  let all_names = names grammar in
  let defined_names = List.map fst grammar |> Sset.of_list in
  let undefined = Sset.diff all_names defined_names in
  let extra_rules =
    let epsilon = [[]] in
    Sset.fold (fun name rules -> (name, epsilon) :: rules) undefined []
  in
  grammar @ extra_rules


(** {2 Conversion to grammars} *)

module Smap = Map.Make(String)

let map_names_to_ids rules =
  let names = List.map fst rules |> Array.of_list in
  let _, indices = Array.fold_left
      (fun (i, map) name -> i + 1, Smap.add name i map)
      (0, Smap.empty)
      names
  in
  names, indices

let product_to_component indices product =
  let add_symbol (atoms, factors) = function
    | Z n -> atoms + n, factors
    | Elem name -> atoms, Grammar.Elem (Smap.find name indices) :: factors
    | Seq name -> atoms, Grammar.Seq (Smap.find name indices) :: factors
  in
  let atoms, factors = List.fold_left add_symbol (0, []) product in
  atoms, List.rev factors

let rule_to_grammar_rule indices (_, terms) =
  List.map (product_to_component indices) terms

let to_grammar g =
  let g = completion g in
  let names, indices = map_names_to_ids g in
  let rules =
    List.map (rule_to_grammar_rule indices) g
    |> Array.of_list
  in
  Grammar.{rules; names}
