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

type t = {names: string array; rules: int expression array}

and 'ref expression =
  | Z of int
  | Product of 'ref expression list
  | Union of 'ref expression list
  | Seq of 'ref expression
  | Ref of 'ref

let product = function
  | [] | [_] ->
    invalid_arg "product with less that two arguments"
  | args ->
    Product args

let union = function
  | [] | [_] ->
    invalid_arg "union with less that two arguments"
  | args ->
    Union args

(* Pretty printing *)

let pp_expression ~pp_ref =
  let rec aux fmt = function
    | Product args ->
      Format.fprintf fmt "(%a)"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.pp_print_string fmt " * ")
           aux )
        args
    | Union args ->
      Format.fprintf fmt "(%a)"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.pp_print_string fmt " + ")
           aux )
        args
    | Seq e ->
      Format.fprintf fmt "Seq(%a)" aux e
    | Ref name ->
      pp_ref fmt name
    | Z i ->
      Format.fprintf fmt "z^%d" i
  in
  aux

let pp fmt {rules; names} =
  Array.iteri
    (fun i expr ->
      Format.fprintf fmt "%s ::= %a@\n" names.(i)
        (pp_expression ~pp_ref:Format.pp_print_int)
        expr )
    rules
