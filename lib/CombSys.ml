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


(** evalution of a node at a given coordinate z *)
let eval_combnode (z:float) (u:float array) (cn:combnode):float = 
	match cn with
		 Z -> z
		|One -> 1.0
		|Refe(i) -> u.(i)

(** evaluation of a product at a given coordinate z *)
let eval_combprod (z:float) (u:float array) (cp:combprod):float = 
	let eval_combnode_s = eval_combnode z u in
	fold_map eval_combnode_s ( *.) 1.0 cp

(** evaluation of an equation at a given coordinate z *)
let eval_eq (z:float) (u:float array) (eq:combeq):float = 
	let eval_combprod_s = eval_combprod z u in
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

let get_length (phi:combsys) : int =
	Array.length phi

let print_combnode (cn:combnode): string list= 
	match cn with
		 Z -> ["Z"]
		|One -> ["One"]
		|Refe(i) -> ["( Refe ";(string_of_int i);")"]

let print_combprod (cp:combprod): unit =
	let l = List.length cp in
	let le = l - 2 in
	let lf = l - 1 in
	print_string "[";
	for i=0 to le do
		printstrlistL (print_combnode (List.nth cp i));
		print_string ";"
	done;
	printstrlistL (print_combnode (List.nth cp lf));
	print_string "]"
	
let print_combeq (ce:combeq): unit =    
	let l = List.length ce in
	let le = l - 2 in
	let lf = l - 1 in  
	print_string "[";
	for i=0 to le do
		print_combprod (List.nth ce i);
		print_string ";"
	done;
	print_combprod (List.nth ce lf);
	print_string "]"
	

let print_combsys (cs:combsys): unit =    
	let l = Array.length cs in
	let le = l - 2 in
	let lf = l - 1 in 
	print_endline "[|";
	for i=0 to le do
		print_combeq cs.(i);
		print_string ";";
		print_endline " "
	done;
	print_combeq cs.(lf);
	print_endline " ";
	print_string "|]";
	print_endline " ";


