(********************************************************
* Arbogen-lib : fast uniform random generation of trees *
*********************************************************
* Module: Ast                                           *
* -------                                               *
* The Abstract syntax tree of the grammar               *
* -------                                               *
* (C) 2011, Xuming Zhan, Frederic Peschanski            *
*           Antonine Genitrini, Matthieu Dien           *
*           Marwan Ghanem                               *
*           under the                                   *
*           GNU GPL v.3 licence (cf. LICENSE file)      *
*********************************************************)

open Util

type expression =
  | Epsilon
  | Z
  | Id of string
  | Union of expression list
  | Prod of expression list
  | Sequence of expression
  | LeqSeq of int * expression
  | EqSeq of int * expression
  | GeqSeq of int * expression

type statement = string * expression

(* Parameter *)
type value = Vint of int | Vfloat of float | Vstring of string
type parameter = Param of string * value

type ast = parameter list * statement list

let rec string_of_expr expr =
  match expr with
  | Epsilon -> "1"
  | Z -> "z"
  | Id s ->  s
  | Union exprs -> "Union(" ^ (Util.string_of_list string_of_expr "" "," "" exprs) ^ ")"
  | Prod exprs -> "Prod(" ^ (Util.string_of_list string_of_expr "" "," "" exprs) ^ ")"
  | Sequence expr -> "Sequence(" ^ (string_of_expr expr) ^ ")"
  | LeqSeq (n, expr) -> "LeqSeq(" ^ (string_of_int n) ^ "," ^ (string_of_expr expr) ^ ")"
  | EqSeq (n, expr) -> "EqSeq(" ^ (string_of_int n) ^ "," ^ (string_of_expr expr) ^ ")"
  | GeqSeq (n, expr) -> "GeqSeq(" ^ (string_of_int n) ^ "," ^ (string_of_expr expr) ^ ")"

let id = ref (-1)
let fresh_id () =
  incr id;
  "_A" ^ (string_of_int !id)


let flatten_expr expr =
  let new_statements = ref [] in
  let rec aux expr =
    match expr with
    | Union exprs ->
      let (exprs', modif) =
        List.fold_left
          (fun (exprs, modif) subexpr ->
             let subexpr', modif' = aux subexpr in
             match subexpr' with
             | Union s -> (s @ exprs, true)
             | e -> (e :: exprs, modif || modif'))
          ([], false) exprs
      in (Union exprs', modif)
    | Prod exprs ->
      let (exprs', modif) =
        List.fold_left
          (fun (exprs, modif) subexpr ->
             let subexpr', modif' = aux subexpr in
             match subexpr' with
             | Prod s -> (s @ exprs, true)
             | Union _ as u ->
               let new_id = fresh_id () in
               new_statements := (new_id, u) :: (!new_statements);
               ((Id new_id) :: exprs, true)
             | e -> (e :: exprs, modif || modif'))
          ([], false) exprs
      in (Prod exprs', modif)
    | Sequence e ->
      (match e with
       | Epsilon -> failwith "Sequence of epsilon is not allowed"
       | Z | Id _ -> (expr, false)
       | _ -> let (e', _) = aux e in
         let new_id = fresh_id () in
         new_statements := (new_id, e') :: (!new_statements);
         (Sequence (Id new_id), true))
    | LeqSeq (n, e) ->
      (match e with
       | Epsilon -> failwith "Sequence of epsilon is not allowed"
       | Z | Id _ ->
         let exprs =
           fold_int (fun acc n ->
               if n = 0 then (Epsilon :: acc)
               else (Prod (fold_int (fun acc n -> if n = 0 then acc else e :: acc) [] n)) :: acc)
             [] n
         in (Union exprs, true)
       | _ -> let (e', _) = aux e in
         let new_id = fresh_id () in
         new_statements := (new_id, e') :: (!new_statements);
         let (new_leq_seq, _) = aux (LeqSeq (n, (Id new_id))) in
         (new_leq_seq, true))
    | EqSeq (n, e) ->
      (match e with
       | Epsilon -> failwith "Sequence of epsilon is not allowed"
       | _ -> aux (Prod (fold_int (fun acc n -> if n = 0 then acc else e :: acc) [] n)))
    | GeqSeq (n, e) ->
      (match e with
       | Epsilon -> failwith "Sequence of epsilon is not allowed"
       | _ -> aux (Prod (fold_int (fun acc n -> if n = 0
                                    then (Sequence e) :: acc
                                    else e :: acc)
                           [] n)))
    | e -> (e, false) in
  let flat_expr, _ = aux expr in
  flat_expr, !new_statements

let rec grm_cmp_of_expr = function
  | Epsilon -> Grammar.Cons (0, [])
  | Z -> Grammar.Cons (1, [])
  | Id s -> Grammar.Call s
  | Prod exprs ->
    let weight, elem_list =
      List.fold_left
        (fun (w, l) expr ->
           match expr with
           | Epsilon -> (w, l)
           | Z -> (w+1, l)
           | Id s -> (w, (Grammar.Elem s) :: l)
           | Sequence e ->
             (match e with
              | Z -> (w, (Grammar.Seq "z") :: l)
              | Id s -> (w, (Grammar.Seq s) :: l)
              | _ -> failwith "Sequence should have been flattened")
           | _ -> failwith "no EqSeq, LeqSeq or GeqSeq expected here")
        (0, []) exprs
    in Grammar.Cons (weight, elem_list)
  | Sequence e ->
    (match e with
     | Z -> Grammar.Cons (0, [(Grammar.Seq "z")])
     | Id s -> Grammar.Cons (0, [(Grammar.Seq s)])
     | _ -> failwith "Sequence should have been flattened")
  | _ -> failwith "no EqSeq, LeqSeq or GeqSeq expected here"

let rec grm_rule_of_stmt (name, expr) =
  match expr with
  | EqSeq _ | LeqSeq _ | GeqSeq _ -> failwith "no EqSeq, LeqSeq or GeqSeq expected here"
  | Union exprs -> (name, List.map grm_cmp_of_expr exprs)
  | _ -> (name, [grm_cmp_of_expr expr])

let grammar_of_ast ast =
  let flattened_stmts =
    List.fold_left (fun acc (name, expr) ->
        let flat_expr, stmt' = flatten_expr expr in
        (name, flat_expr) :: (stmt' @ acc))
      [] ast in
  Grammar.completion (List.map grm_rule_of_stmt flattened_stmts)
