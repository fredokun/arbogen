(*********************************************************
 * Arbogen-lib : fast uniform random generation of trees *
 *********************************************************
 * Module: Tree                                          *
 * -------                                               *
 * Internal representation of trees and export tools     *
 * -------                                               *
 * (C) 2011, Xuming Zhan, Frederic Peschanski            *
 *           Antonine Genitrini under the                *
 *           GNU GPL v.3 licence (cf. LICENSE file)      *
 *********************************************************)


open Util

type tree = Leaf of string | Node of string * (tree list)

let tree_leaf (s:string) : tree = 
	(Leaf s)

let tree_Node (s:string) (tl:tree list) : tree = 
	Node(s,tl)
(*
let rec tree_print (thetree:tree) : unit = 
	match thetree with
		Leaf thest ->
			let tree_leafp (Leaf st):unit = 
				print_string " Leaf :";
				print_string " '' ";
				print_string st;
				print_string " '' "
			in
			tree_leafp thetree
		|Node(thest,thetl) ->
			let tree_nodep (Node(st,tl)):unit = 
				let nl = List.length tl in
				print_string " Node ( '' ";
				print_string st;
				print_string " '', [ ";
				let lm1 = nl - 1 in
				let lm2 = nl - 2 in
				for var = 0 to lm2 do
					tree_print (List.nth tl var);
(* ne faire que si var < nl*)
					print_string " ; ";
				done;
				tree_print (List.nth tl lm1);
				print_endline "])"
			in
			tree_nodep thetree;
*)
let rec tree_print (thetree:tree) (bl:int) : unit = 
	match thetree with
		Leaf thest ->
			let tree_leafp (Leaf st):unit = 
				printblanc (bl-1);
				print_string st
			in
			tree_leafp thetree
		|Node(thest,thetl) ->
			let tree_nodep (Node(st,tl)):unit = 
				let nl = List.length tl in
				print_string "  Node :";
				print_endline st;
				let lm1 = nl - 1 in
				let lm2 = nl - 2 in
				for var = 0 to lm2 do
					printblanc bl;
					tree_print (List.nth tl var) (bl+1);
(* ne faire que si var < nl*)
					print_endline ";";
					(*print_endline ";";*)
				done;
				printblanc bl;
				tree_print (List.nth tl lm1) (bl+1);
				print_endline ""
			in
			tree_nodep thetree;

			

			
			
	
	
