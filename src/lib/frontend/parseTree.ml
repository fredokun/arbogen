type symbol =
  | Z of int
  | Elem of string
  | Seq of string
type product = symbol list
type rule = string * product list
type t = rule list


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
