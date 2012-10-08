(**********************************************************
 * Arbogen-tool : fast uniform random generation of trees *
 **********************************************************
 * Module: GParser                                        *
 * -------                                                *
 * Tree grammar parser                                    *
 * -------                                                *
 * (C) 2011, Xuming Zhan, Frederic Peschanski             *
 *           Antonine Genitrini under the                 *
 *           GNU GPL v.3 licence (cf. LICENSE file)       *
 **********************************************************)

open Printf

open Util
open Options

exception Parse_Error of string;;

type character =
    Char of char
  | EOF;;

let is_space = function
  | Char(ch) -> ch == ' ' or ch == '\n' or ch == '\t' or ch == '\r'
  | EOF -> false ;;

let get_char str i = 
  try Char (String.get str i) with
      _ -> EOF;;

let rec skip_spaces str i =
  let ichar = get_char str i in
  if is_space ichar then skip_spaces str (i+1)
  else i ;;

let rec skip_until_eol str i =
  let ichar = get_char str i in
  match ichar with
    | EOF -> i
    | Char(ch) -> 
      if ch='\n' then i+1
      else skip_until_eol str (i+1);;

let rec skip_until_starslash str i =
  let ichar = get_char str i in
  match ichar with
    | EOF -> raise (Parse_Error "Missing end of comment: */") 
    | Char(ch) -> if ch='*' then (match get_char str (i+1) with
        | EOF -> raise (Parse_Error "Missing end of comment after *") 
        | Char(ch') -> 
          if ch'='/' then i+2
          else skip_until_starslash str (i+1))
      else skip_until_starslash str (i+1) ;;

(* skip_until_starslash "/* toto titi tata */ tutu" 2 ;; *)

let rec skip_comments str i =
  let ichar = get_char str i in
  match ichar with
    | EOF -> i
    | Char(ch) -> 
      if is_space ichar then skip_comments str (i+1)
      else if ch = '/' then
        (match get_char str (i+1) with
          | EOF -> i
          | Char(ch') -> 
            if ch' = '/' then skip_until_eol str i 
            else if ch' = '*' then skip_until_starslash str (i+1)
            else i)
      else i;;

(* skip_comments "/* toto titi tata */ tutu" 0 ;;  *)

let rec skip str i =
  let i' = skip_comments str i in
  if not (i'==i) then skip str i'
  else i;;

let list_iteri f l =
  let rec aux i l = match l with 
    | [] -> ()
    | e::l' -> (f i e) ; aux (i+1) l'
  in
  aux 0 l ;;

let string_of_list l = 
  let len = List.length l in
  let str = String.create len
  in 
  list_iteri (fun ch i -> String.set str ch i) l ;
  str ;;

let next_word str i =
  let rec aux i word =
    let ichar = get_char str i in
    match ichar with
      | EOF -> (List.rev word,i)
      | Char(ch) -> 
        if is_space ichar then
          (List.rev word,i)
        else if ch='*' or ch='+' or ch=';' then
          (match word with
            | [] -> ([ch], i+1)
            | _ -> (List.rev word, i))
        else aux (i+1) (ch::word)
  in
  let (word,i') = aux (skip str i) [] in
  (* print_endline ("next word = " ^ (string_of_list word)) ; *)
  (string_of_list word,i') ;;

let advance str i expect = 
  let (word,i') = next_word str i
  in
  if word = expect then i'
  else raise (Parse_Error ("Missing '" ^ expect ^ "'")) ;;

let parse_component str i =
  let rec aux i weight refs =
    let (componentName,i') = next_word str i in
    (* print_endline ("Component name = " ^ componentName) ; *)
    if componentName="<z>" then
      let (next,i'') = next_word str i' in
      (if next="*" then
          aux i'' (weight+1) refs
       else if next="+" or next=";" then
         match refs with
           | [] -> raise (Parse_Error "Missing reference after <z>")
           | _ -> ((weight+1,List.rev refs),i')
       else
         raise (Parse_Error "Expecting '*', '+' or ';' after <z>"))
    else if componentName="*" or componentName="+" then
      raise (Parse_Error ("Unexpected '" ^ componentName ^ "'"))
    else (* component Name is ok *)
      let (next,i'') = next_word str i' in
      (* print_endline ("Next = " ^ next) ; *)
      if next="+" or next =";" then
        ((weight,List.rev ((ELEM componentName)::refs)),i')
      else if next="*" then
        aux i'' weight ((ELEM componentName)::refs)
      else raise (Parse_Error "Expecting '+', ';' or '*'") 
  in
  aux i 0 [] ;;

(* parse_component "<z> * BinNode * BinNode +" 0 ;; *)

let parse_components str i =
  let rec aux i comps =
    let (comp,i') = parse_component str i in
    let (next,i'') = next_word str i' in
    if next="+" then
      aux i'' (comp::comps)
    else if next=";" then
      (List.rev (comp::comps), i'')
    else raise (Parse_Error ("Expecting '+' or ';' after component"))
  in
  aux i [] ;;

(* parse_components "Leaf * <z> + BinNode * BinNode ;" 0 ;;  *)
      
let parse_rule str i =
  let (ruleName,i') = next_word str i in
  if ruleName="" then
    raise (Parse_Error "Missing rule name")
  else
    let i'' = advance str i' "::=" in
    let (components,i''') = parse_components str i''
    in
    ((ruleName,components),i''') ;;

(* parse_rule "BinNode ::= Leaf * <z> + BinNode * BinNode ;" 0 ;; *)


let parse_int str i =
  let int_str, i' = next_word str i
  in
    try
      let int_val = int_of_string int_str
      in
        (int_val, i')
    with Failure _ -> raise (Parse_Error (sprintf "cannot convert '%s' to an integer" int_str))

let parse_float str i =
  let float_str, i' = next_word str i
  in
    try
      let float_val = float_of_string float_str
      in
        (float_val, i')
    with Failure _ -> raise (Parse_Error (sprintf "cannot convert '%s' to a float" float_str))


let parse_option str i =
  let opt_id, i' = next_word str i
  in match opt_id with
  | "min" -> 
    let min_val, i' = parse_int str i'
    in
      (if min_val < 0 
       then raise (Option_Error (sprintf "incorrect minimal size %d => should be positive" min_val))
       else if not global_options.size_min_set 
       then global_options.size_min <- min_val) ;
      advance str i' ";"
  | "max" -> 
    let max_val, i' = parse_int str i'
    in
      (if max_val < 0 
       then raise (Option_Error (sprintf "incorrect maximal size %d => should be positive" max_val))
       else if not global_options.size_max_set
       then global_options.size_max <- max_val) ;
      advance str i' ";"
  | "try" -> 
    let try_val, i' = parse_int str i'
    in
      (if try_val <= 0 
       then raise (Option_Error (sprintf "incorrect minimal try number %d => should be strictly positive" try_val))
       else if not global_options.max_try_set 
       then global_options.max_try <- try_val) ;
      advance str i' ";"
  | "eps1" | "epsilon1" ->
    let eps1_val, i' = parse_float str i'
    in
      (if eps1_val <= 0.0
       then raise (Option_Error (sprintf "incorrect epsilon 1 %f => should be strictly positive" eps1_val))
       else if not global_options.epsilon1_set
       then global_options.epsilon1 <- eps1_val) ;
      advance str i' ";"
  | "eps1_factor" | "epsilon1_factor" ->
    let eps1_val, i' = parse_float str i'
    in
      (if eps1_val <= 0.0
       then raise (Option_Error (sprintf "incorrect epsilon 1 factor %f => should be strictly positive" eps1_val))
       else if not global_options.epsilon1_factor_set
       then global_options.epsilon1_factor <- eps1_val) ;
      advance str i' ";"
  | "eps2" | "epsilon2" ->
    let eps2_val, i' = parse_float str i'
    in
      (if eps2_val <= 0.0
       then raise (Option_Error (sprintf "incorrect epsilon 2 %f => should be strictly positive" eps2_val))
       else if not global_options.epsilon2_set
       then global_options.epsilon2 <- eps2_val) ;
      advance str i' ";"
  | "eps2_factor" | "epsilon2_factor" ->
    let eps2_val, i' = parse_float str i'
    in
      (if eps2_val <= 0.0
       then raise (Option_Error (sprintf "incorrect epsilon 2 factor %f => should be strictly positive" eps2_val))
       else if not global_options.epsilon2_factor_set
       then global_options.epsilon2_factor <- eps2_val) ;
      advance str i' ";"
  | _ -> raise (Parse_Error (sprintf "Uknown or unsupported option: %s" opt_id))
    
    
let parse_grammar str =
  let rec aux i rules =
    match next_word str i with
      ("",_) -> List.rev rules
    | ("set", i') -> 
      let i'' = parse_option str i'
      in
        aux i'' rules
    | _ ->
      let (rul,i') = parse_rule str i
      in aux i' (rul::rules)
  in
    aux 0 [] ;;

(* parse_grammar "BinNode ::= Leaf * <z> + BinNode * BinNode;" ;; *)

let string_of_file fname =
  let inchan = open_in fname in
  let rec read str =
    try let next = input_line inchan in
        read (str ^ "\n" ^ next)
    with End_of_file -> close_in inchan ; str
  in
  read "" ;;

let parse_from_file fname =
  let input = string_of_file fname in
  parse_grammar input ;;

(*
let gram = parse_from_file "examples/binary.arb" 
in 
(print_endline "Grammar parsed = ") ;
(print_endline (string_of_grammar gram));;
*)
