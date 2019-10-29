(*********************************************************
 * Arbogen-lib : fast uniform random generation of trees *
 *********************************************************
 * Module: Grammar                                       *
 * -------                                               *
 * Internal representation of grammars                   *
 * -------                                               *
 * (C) 2011, Xuming Zhan, Frederic Peschanski            *
 *           Antonine Genitrini, Matthieu Dien           *
 *           Marwan Ghanem                               *
 *           under the                                   *
 *           GNU GPL v.3 licence (cf. LICENSE file)      *
 *********************************************************)

type t = {names: string array; rules: rule array}

and rule = component list
and component = int * elem list
and elem =
  | Elem of int
  | Seq of int

let epsilon = (0, [])


(* Conversion from parseTree *)

module ParseTree = Frontend.ParseTree
module Smap = Map.Make(String)

let map_names_to_ids rules =
  let names = List.map fst rules |> Array.of_list in
  let _, indices = Array.fold_left
      (fun (i, map) name -> i + 1, Smap.add name i map)
      (0, Smap.empty)
      names
  in
  names, indices

let component_of_product indices product =
  let add_symbol (atoms, factors) = function
    | ParseTree.Z n -> atoms + n, factors
    | ParseTree.Elem name -> atoms, Elem (Smap.find name indices) :: factors
    | ParseTree.Seq name -> atoms, Seq (Smap.find name indices) :: factors
  in
  let atoms, factors = List.fold_left add_symbol (0, []) product in
  atoms, List.rev factors

let rule_of_parsetree_rule indices (_, terms) =
  List.map (component_of_product indices) terms

let of_parsetree t =
  let t = ParseTree.completion t in
  let names, indices = map_names_to_ids t in
  let rules =
    List.map (rule_of_parsetree_rule indices) t
    |> Array.of_list
  in
  {rules; names}


(* Pretty printing *)

let pp_elem names fmt = function
  | Elem i -> Format.pp_print_string fmt names.(i)
  | Seq i -> Format.fprintf fmt "Seq(%s)" names.(i)

let pp_product pp_term fmt terms =
  let pp_sep fmt () = Format.fprintf fmt " * " in
  match terms with
  | [] -> Format.fprintf fmt "1"
  | _ -> Format.pp_print_list ~pp_sep pp_term fmt terms

let pp_component names fmt (weight, elems) =
  if weight <> 0 then
    Format.fprintf fmt "z^%d * " weight;
  pp_product (pp_elem names) fmt elems

let pp_union pp_term fmt terms =
  let pp_sep fmt () = Format.fprintf fmt " + " in
  match terms with
  | [] -> Format.fprintf fmt "0"
  | _ -> Format.pp_print_list ~pp_sep pp_term fmt terms

let pp fmt {rules; names} =
  Array.iteri
    (fun i rule ->
       Format.fprintf fmt "%s ::= %a@\n"
         names.(i)
         (pp_union (pp_component names)) rule)
    rules
