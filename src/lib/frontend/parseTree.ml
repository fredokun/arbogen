type t = rule list
and rule = string * product list
and product = atomic list

and atomic =
  | Z of int
  | Elem of string
  | Seq of string

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

let unbound_symbols grammar =
  let all_names = names grammar in
  let bound_names = List.map fst grammar |> Sset.of_list in
  Sset.diff all_names bound_names

let is_complete grammar =
  Sset.is_empty (unbound_symbols grammar)

let completion grammar =
  let unbound = unbound_symbols grammar in
  let extra_rules =
    let epsilon = [[]] in
    Sset.fold (fun name rules -> (name, epsilon) :: rules) unbound []
  in
  grammar @ extra_rules


(** {2 Pretty-printing} *)

let pp_atomic fmt = function
  | Z n -> Format.fprintf fmt "z^%d" n
  | Elem name -> Format.pp_print_string fmt name
  | Seq name -> Format.fprintf fmt "Seq(%s)" name

let pp_product =
  let pp_sep fmt () = Format.pp_print_string fmt " * " in
  Format.pp_print_list ~pp_sep pp_atomic

let pp_derivations =
  let pp_sep fmt () = Format.pp_print_string fmt " + " in
  Format.pp_print_list ~pp_sep pp_product

let pp_rule fmt (name, derivations) =
  Format.fprintf fmt "%s ::= %a" name pp_derivations derivations

let pp =
  let pp_sep fmt () = Format.fprintf fmt "@\n" in
  Format.pp_print_list ~pp_sep pp_rule
