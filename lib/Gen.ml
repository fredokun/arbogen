(*********************************************************
 * Arbogen-lib : fast uniform random generation of trees *
 *********************************************************
 * Module: Gen                                           *
 * -------                                               *
 * The Boltzmann random generator                        *
 * -------                                               *
 * (C) 2011, Xuming Zhan, Frederic Peschanski            *
 *           Antonine Genitrini, Matthieu Dien           *
 *           under the                                   *
 *           GNU GPL v.3 licence (cf. LICENSE file)      *
 *********************************************************)

open Tree

open Util

open CombSys

open Grammar

open OracleSimple

(* g must be completed
Renvoie une map des poids total de chaque composant (somme des pondération des sous composants)
et une map de la grammaire sous forme de (composant -> liste des (liste des sous_composants * pondération)) *)
let pondere (g:grammar) (y:float array)
	(*: (float StringMap * (string list * float) list StringMap )*) =
	let ymap = List.fold_left2 (fun map coef (name,_) -> StringMap.add name coef map) (StringMap.empty) (Array.to_list y) g
	in
	(* calcule la pondération correspondante à un sous composants *)
	let aux2 = fun (_,componentList) ->
		let proba = List.fold_left (fun a b -> a *. (StringMap.find b ymap)) 1. componentList  in (componentList,proba)
	in
	(* renvoie la map des composants avec leurs sous composants (prochain fils) et pondération *)
	let aux = fun (map,wmap) (component,subComponents) ->
		let sub_component_list = List.map aux2 subComponents in
		(StringMap.add component sub_component_list map,
		 StringMap.add component (List.fold_left (fun a (_,f) -> f +. a) 0. sub_component_list) wmap)
	in
	let (gmap,wmap) = List.fold_left aux (StringMap.empty,StringMap.empty) g
	in
	(wmap,gmap)

(* StringMap.iter (fun x l -> print_string (x^" -> ") ; List.iter (fun (l',f) -> print_float f ; List.iter (fun e->print_string  (" "^e^";")) l') l ; print_endline "" ) gmap ;; *)


let gen_tree
	(g:grammar)
	(with_prefix:bool) (idprefix:string)
	(sizemax:int)
	(y:float array) : (tree option * int) =
	let (first_rule,_) = List.hd g in
	let (wmap,gmap) = pondere g y in
	let rec gen_tree_rec (size:int) (next_rule:string) : (tree option * int) =
		if sizemax-size<=0 then
			None
		else
		(* On génère la suite de l'arbre *)
			let prefix = if with_prefix then idprefix ^ (string_of_int (size+1)) else (string_of_int (size+1)) in
			if StringMap.find next_rule wmap = 1. then
				(* On doit générer une feuille *)
				(Some (Leaf(next_rule,prefix)),size+1)
			else
				(* On doit générer des sous arbres *)
				let rdm_float = Random.float (StringMap.find next_rule wmap) in
				let (_,_,next_rules_list) = List.fold_left
				(fun (limit,stop,temp) (l,f) -> if limit-.f<=0. && stop then (limit,false,l) else (limit-.f,stop,temp) )
				(rdm_float,true,[])
				(StringMap.find next_rule gmap) in
				let aux opt next =
					match opt with
						|None -> None
						|Some(l,s) -> match gen_tree_rec s next with
							|None -> None
							|(Some sub_tree,new_size) -> Some(l@[sub_tree],new_size)
				in
				let suite = List.fold_left aux Some([],size+1) next_rules_list in
				if suite = None then (None,sizemax)
				else let Some(sons,s)=suite in (Some(Node(next_rule,prefix,sons)),s)
	in
	gen_tree_rec 0 first_rule

(* TODO: à documenter *)
let generator 
	(g:grammar)
	(self_seed:bool) (seed:int)
	(sizemin:int) (sizemax:int)
	(epsilon1:float) (epsilon1_factor:float)
	(epsilon2:float) (epsilon2_factor:float)
	(with_prefix:bool) (idprefix:string)
	(max_try:int) (ratio_rejected:float)
	(max_refine:int) : (tree*int) option =
	(if self_seed
	then Random.self_init ()
	else Random.init seed) ;
	let sys = combsys_of_grammar (completion g) in
	let rec gen epsilon1 epsilon2 zmin zmax nb_refine =
		let (zmin',zmax',y) = searchSingularity sys zmin zmax epsilon1 epsilon2 in
		(*Array.iter (fun e -> print_endline (string_of_float e)) y;*)
		let rec try_gen (nb_try:int) (nb_smaller:int) (nb_bigger:int) : ((tree * int) option * int * int) =
			if nb_try > 0 then
				(match gen_tree g with_prefix idprefix sizemax y with
				| (Some tree,size) ->
					print_endline (string_of_tree tree);
					if size<sizemin then
					try_gen (nb_try-1) (nb_smaller+1) nb_bigger
					else (Some (tree,size), nb_smaller, nb_bigger)
				| (None,_) -> try_gen (nb_try-1) nb_smaller (nb_bigger+1))
			else (* max number of tries *)
				(None,nb_smaller,nb_bigger)
		in
		if nb_refine<=max_refine then
		(let (ptree,nb_smaller,nb_larger) = try_gen max_try 0 0 in
		match ptree with
			| Some _ -> ptree (* ok, found a tree, return it with its size *)
			| None ->
			if (float_of_int nb_smaller) /. (float_of_int (nb_smaller+nb_larger)) >= ratio_rejected
			then (* if more than e.g. 80% of the trees are too small, then refine *)
				gen (epsilon1 *. epsilon1_factor) (epsilon2 *. epsilon2_factor) zmin' zmax' (nb_refine+1)
			else failwith "Normally you shouldn't be here, so report this bug please")
		else None (* refined too much : could not generate a tree *)
	in
	gen epsilon1 epsilon2 0. 1. 1

