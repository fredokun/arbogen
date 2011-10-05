(*********************************************************
 * Arbogen-lib : fast uniform random generation of trees *
 *********************************************************
 * Module: Grammar                                       *
 * -------                                               *
 * Internal representation of grammars                   *
 * -------                                               *
 * (C) 2011, Xuming Zhan, Frederic Peschanski            *
 *           Antonine Genitrini under the                *
 *           GNU GPL v.3 licence (cf. LICENSE file)      *
 *********************************************************)

open Util
open CombSys
open Tree

(** a grammar rule is either a terminal (leaf) node or a non-terminal node *)
type rule = Terminal | NonTerminal of (string list list)

let n_rule (r:rule):int = match r with
	Terminal -> 0	
	|NonTerminal ll -> (List.length ll)

(** a grammar is a list of named, indexed rules *)
type grammar = (string * int * rule) list

let grastr (g:grammar) (i:int) : string = 
	let (p1,_,_) = List.nth g i in
	p1

let graint (g:grammar) (i:int) : int = 
	let (_,p2,_) = List.nth g i in
	p2

let grarule (g:grammar) (i:int) : rule = 
	let (_,_,p3) = List.nth g i in
	p3

let gra_findWeight (g:grammar) (r:string) : int = 
	let predica ((st,_,_):string*int*rule):bool =
		if String.compare r st = 0 then
			true
		else
			false
	in
	let rl = List.filter predica g in
	let (_,p2,_) = List.hd rl in
	p2

let rec tree_size (thetree:tree) (g:grammar) : int = 
	match thetree with
		Leaf thest ->
			let weileaf = gra_findWeight g thest in
			weileaf
		|Node(thest,thetl) ->
			let weinode = gra_findWeight g thest in
			let res = ref weinode in
			let l = List.length thetl in
			let theend = l - 1 in
			for i = 0 to theend do
				res := !res + (tree_size (List.nth thetl i) g)
			done;
		!res

	

let gra_findrule (g:grammar) (r:string) : rule = 
	let predica ((st,_,_):string*int*rule):bool =
		if String.compare r st = 0 then
			true
		else
			false
	in
	let rl = List.filter predica g in
	let (_,_,p3) = List.hd rl in
	p3



let gra_findTer (g:grammar) : string array = 
	let predica ((_,_,ru):string*int*rule):bool =
		match ru with
			Terminal -> true
			| NonTerminal _ -> false
	in 
	let rl = List.filter predica g in
	let lrl = List.length rl in
	let r = Array.make lrl "name" in
	let theend = lrl - 1 in
	for i = 0 to theend do
		let (p1,_,_) = List.nth rl i in
		r.(i) <- p1
	done;
	r;


		
	
	

	 
	
		


					
				
				
						

	
