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

(* {2 Grammar internal representation} *)

(** A grammar is an array of production rules and a mapping from integers
    to non-terminal names *)
type t = {names: string array; rules: rule array}

and rule = component list
and component = int * elem list
and elem =
  | Elem of int
  | Seq of int

let epsilon = (0, [])


(** {2 Pretty printing} *)

let pp_elem names fmt = function
  | Elem i -> Format.fprintf fmt "Elem(%s)" names.(i)
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
