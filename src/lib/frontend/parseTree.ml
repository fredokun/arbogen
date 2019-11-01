type t = rule list
and rule = string * string Grammar.expression

(** {2 Grammar completion} *)

module Sset = Set.Make(String)

let rec expr_names set = function
  | Grammar.Product (e1, e2) -> expr_names (expr_names set e1) e2
  | Grammar.Union (e1, e2) -> expr_names (expr_names set e1) e2
  | Grammar.Seq e -> expr_names set e
  | Grammar.Z _ -> set
  | Grammar.Reference name -> Sset.add name set

let names grammar =
  List.fold_left
    (fun set (_, expr) -> expr_names set expr)
    Sset.empty
    grammar

let unbound_symbols grammar =
  let all_names = names grammar in
  let bound_names = List.map fst grammar |> Sset.of_list in
  Sset.diff all_names bound_names

let is_complete grammar =
  Sset.is_empty (unbound_symbols grammar)

let completion grammar =
  let unbound = unbound_symbols grammar in
  let extra_rules =
    let epsilon = Grammar.Z 0 in
    Sset.fold (fun name rules -> (name, epsilon) :: rules) unbound []
  in
  grammar @ extra_rules


(* Conversion to Grammars *)

module Smap = Map.Make(String)

let map_names_to_ids rules =
  let names = List.map fst rules |> Array.of_list in
  let _, indices = Array.fold_left
      (fun (i, map) name -> i + 1, Smap.add name i map)
      (0, Smap.empty)
      names
  in
  names, indices

let expr_to_id indices =
  let open Grammar in
  let rec aux = function
    | Z n -> Z n
    | Product (e1, e2) -> Product (aux e1, aux e2)
    | Union (e1, e2) -> Union (aux e1, aux e2)
    | Seq e -> Seq (aux e)
    | Reference r -> Reference (Smap.find r indices)
  in
  aux

let to_grammar t =
  let t = completion t in
  let names, indices = map_names_to_ids t in
  let rules =
    t
    |> List.map (fun (_, expr) -> expr_to_id indices expr)
    |> Array.of_list
  in
  Grammar.{rules; names}


(** {2 Pretty-printing} *)

let pp_rule fmt (name, expr) =
  let pp_expr = Grammar.pp_expression ~pp_ref:Format.pp_print_string in
  Format.fprintf fmt "%s ::= %a" name pp_expr expr


let pp =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
    pp_rule
