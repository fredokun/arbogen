(*********************************************************
 * Arbogen-lib : fast uniform random generation of trees *
 *********************************************************
 * Module: CombSys                                       *
 * -------                                               *
 * Internal representation of a system of                *
 * combinatorial equations.                              *
 * -------                                               *
 * (C) 2011, Xuming Zhan, Frederic Peschanski            *
 *           Antonine Genitrini under the                *
 *           GNU GPL v.3 licence (cf. LICENSE file)      *
 *********************************************************)

open Util

(* a system is an array of equations *)
type combsys = combeq array 
(* an equation is a list of products *)
and combeq = combprod list
(* a product is a list of nodes *)
and combprod = combnode list
(* a node is either *)
and combnode =
  | Z            (* an instance of the variable Z *)
  | One          (* a unit 1 for the product *)
  | Refe of int  (* a reference to another equation *) 


let combsys_size = Array.length

(** evalution of a node at a given coordinate z *)
let eval_combnode (z:float) (y:float array) (cn:combnode):float = 
	match cn with
		 Z -> z
		|One -> 1.0
		|Refe(i) -> y.(i)

(** evaluation of a product at a given coordinate z *)
let eval_combprod (z:float) (y:float array) (cp:combprod):float = 
	let eval_combnode_s = eval_combnode z y in
	fold_map eval_combnode_s ( *.) 1.0 cp

(** evaluation of an equation at a given coordinate z *)
let eval_eq (z:float) (y:float array) (eq:combeq):float = 
	let eval_combprod_s = eval_combprod z y in
	fold_map eval_combprod_s (+.) 0.0 eq
	 
(** evaluation of a system at a given coordinate z *)
let evaluation (phi:combsys) (z:float) (y:float array):float array = 
	let u = Array.create (Array.length y) 0.0 in
	for i=0 to ((Array.length y) - 1)
	do
		(*print_string ("i=" ^ (string_of_int i)) ;*)
		let vali = eval_eq z y phi.(i)
		in
		  (*print_endline ("  vali=" ^ (string_of_float vali)) ;*)
		  u.(i) <- vali
	done;
	(*print_endline ("u = " ^ (Util.string_of_array string_of_float u)) ;*)
	u 

(* conversion from grammar *)

let rec make_z = function
  | 0 -> [One]
  | n -> Z::(make_z (- n 1)) ;;

let rec make_refs map refs = match refs with
  | [] -> []
  | ref::refs' -> (Refe (StringMap.find ref map))::(make_refs map refs') ;;

let comprod_of_component map (weight,refs) = 
  (make_refs map refs) @ (make_z map weight)

let combeq_of_rule map (_,comps) = 
  List.fold_left (fun eqs comp -> (combprod_of_component map comp)::eqs) [] comps ;;

let refmap_of_grammar grm =
  let rec aux i grm map = match grm with
    | [] -> map
    | (rname,_)::grm' -> aux (i+1) grm' (StringMap.add rname i map)
  in
  aux 0 grm StringMap.empty ;;

let combsys_of_grammar grm =
  let rec aux grm map sys = match grm with
    | [] -> sys
    | ((rname,rbody) as rule)::grm' -> 
      let index = StringMap.find rname map in
      Array.set sys index (combeq_of_rule map rule) ;
      aux grm' map sys
  in
  aux grm (refmap_of_grammar grm) (Array.create (List.length grm)) ;;


(* printing *)

let string_of_combnode = function 
  | Z -> "z"
  | One -> "1"
  | Refe i -> "Ref[" ^ (string_of_int i) ^ "]" ;;

let rec string_of_combprod = function
  | [] -> ""
  | [cn] -> string_of_combnode cn
  | cn::ps -> (string_of_combnode cn) ^ "*" ^ (string_of_compprod ps)

let rec string_of_combeq = function
  | [] -> ""
  | [cp] -> string_of_combprod cp
  | cp::eqs -> (string_of_combprod cp) ^ "+" ^ (string_of_combeq eqs)

let string_of_combsys sys =
  Array.fold_it (fun i e str -> str ^ (string_of_int i) ^ " ==> " ^ (string_of_combeq e) ^ "\n") sys "" ;;

