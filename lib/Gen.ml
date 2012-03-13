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
		let proba = List.fold_left (fun a b -> a *. (StringMap.find b ymap)) 1. componentList  in
		(componentList,proba)
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

let pondere2 (g:grammar) (y:float array)
	(*: (float StringMap * (string list * float) list StringMap )*) =
	let g_comp = completion g in
	let ymap =
		List.fold_left2
		(fun map coef (name,_) -> StringMap.add name coef map)
		(StringMap.empty)
		(Array.to_list y)
		g_comp
	in
	(* calcule la pondération correspondante à un sous composants *)
	let aux2 = fun (_,componentList) ->
		let proba =
			List.fold_left
			(fun a b -> a *. (StringMap.find b ymap))
			1.
			componentList
		in
		let leafs = Grammar.leafs_of_grammar g in
		let len = match componentList with
			|[]->0
			|[a]->if (List.mem a leafs) then 0 else 1
			|_->List.length componentList
		in
		(componentList,len,proba)
	in
	(* renvoie la map des composants avec leurs sous composants (prochain fils) et pondération *)
	let aux = fun map (component,subComponents) ->
		let sub_component_list = List.map aux2 subComponents
		in
		let proba = (List.fold_left (fun a (_,_,f) -> f +. a) 0. sub_component_list)
		in
		StringMap.add
		component
		(sub_component_list,proba)
		map
	in
	List.fold_left aux StringMap.empty g_comp

(* StringMap.iter (fun x l -> print_string (x^" -> ") ; List.iter (fun (l',f) -> print_float f ; List.iter (fun e->print_string  (" "^e^";")) l') l ; print_endline "" ) gmap ;; *)

let rec gen_stack_tree
	size
	next_rules current_rules
	map
	sizemax
	leafs =
	(*Queue.iter (fun elt -> print_string (elt^" ")) next_rules;
	print_endline " ";*)
	if size<sizemax then
		if (Queue.is_empty next_rules) then
			(current_rules,size)
		else
			let folder = fun (limit,stop,temp,b) (l,n,f) ->
				if limit-.f<=0. && stop
					then (limit,false,l,n)
				else
					(limit-.f,stop,temp,b)
			in
			let next_rule = Queue.pop next_rules in
			let (sub_component_list,max_rdm) = StringMap.find next_rule map in
			let rdm_float = Random.float max_rdm in
			let (_,_,next_rules_list,arity) =
				List.fold_left
				folder
				(rdm_float,true,[],0)
				sub_component_list
			in (*Trouves les futurs composants et leur nombre *)
			(*print_endline (string_of_int arity);*)
			List.iter (fun elt -> Queue.push elt next_rules) next_rules_list;
			if not(arity=0) || (arity=0 && (List.mem next_rule leafs)) then
					Stack.push (next_rule,arity) current_rules;
			gen_stack_tree
			(size+1)
			next_rules current_rules
			map
			sizemax
			leafs
	else
		(Stack.create (),0)


let gen_tree_of_stack
	(stack,size)
	with_prefix idprefix =
	let s = ref size in
	let current_rules = Queue.create () in
	(*let print_type t =
			match t with
			|Node(rule,_,_)->print_endline rule
			|Leaf(rule,_)->print_endline rule
			in*)
	while not(Stack.is_empty stack) do
		let prefix =
			if with_prefix then idprefix ^ (string_of_int (!s))
			else (string_of_int (!s))
		in
		s:=!s-1;
		let (rule,arity) = Stack.pop stack in
		if arity=0
			then Queue.push (Leaf(rule,prefix)) current_rules
		else
			(*(
			print_endline "begin";
			Queue.iter print_type current_rules;
			print_endline "end";*)
			let sons = npop arity current_rules in
			(*Queue.iter print_type current_rules;*)
			Queue.push (Node(rule,prefix,sons)) current_rules 
	done;
	(*let t = npop ((Queue.length current_rules)-1) current_rules in
	let root =
		match Queue.pop current_rules with
		|Leaf(e,_)->e
		|Node(e,_,_)->e
	in*)
	(*print_endline "begin";
	Queue.iter print_type current_rules;*)
	if size=0
		then (None,0)
	else
		(*(Some((Node(root,(string_of_int 0),t))),1+size)*)
		(Some(Queue.pop current_rules),size)

let rec gen_tree_rec
	(size:int)
	(next_rule:string)
	wmap gmap sizemax with_prefix idprefix
	: (tree option * int) =
	if sizemax-size<=0 then
		(None,sizemax)
	else
		(* On génère la suite de l'arbre *)
		if StringMap.find next_rule wmap = 1.
			(* On doit générer une feuille *)
			then
					let prefix =
						if with_prefix then idprefix ^ (string_of_int (size))
						else (string_of_int (size))
					in
			(Some (Leaf(next_rule,prefix)),size+1)
		else
			(* On doit générer des sous arbres *)
			let prefix =
				if with_prefix then idprefix ^ (string_of_int (size+1))
				else (string_of_int (size+1))
			in
			let rdm_float = Random.float (StringMap.find next_rule wmap) in
			let (_,_,next_rules_list) =
				List.fold_left
				(fun (limit,stop,temp) (l,f) -> if limit-.f<=0. && stop then (limit,false,l) else (limit-.f,stop,temp))
				(rdm_float,true,[])
				(StringMap.find next_rule gmap)
			in
			let aux opt next =
				match opt with
					|None -> None
					|Some(l,s) ->
						match gen_tree_rec s next wmap gmap sizemax with_prefix idprefix with
							|(None,_) -> None
							|(Some sub_tree,new_size) -> Some(l@[sub_tree],new_size)
			in
			let suite = List.fold_left aux (Some([],size+1)) next_rules_list in
			match suite with
				|None -> (None,sizemax)
				|Some([Leaf(a,b)],s) -> (Some(Leaf(a,b)),s-1)
				|Some(sons,s) -> (Some(Node(next_rule,prefix,sons)),s)

let gen_tree
	(g:grammar)
	(with_prefix:bool) (idprefix:string)
	(sizemax:int)
	(y:float array) : (tree option * int) =
	(*let (first_rule,_) = List.hd g in
	let (wmap,gmap) = pondere (completion g) y in
	gen_tree_rec 0 first_rule wmap gmap sizemax with_prefix idprefix*)
	let map = pondere2 g y in
	let leafs = leafs_of_grammar g in
	(*StringMap.iter
	(fun key (l,_)-> print_endline key; print_endline (string_of_int (List.length l));
	List.iter (fun (_,a,_) -> print_endline (string_of_int a)) l ) map;*)
	let queue = Queue.create () in
	let (first_rule,_) = List.hd g in
	(*print_endline first_rule;*)
	Queue.push first_rule queue;
	let (stack,size) = gen_stack_tree 0 queue (Stack.create ()) map sizemax leafs in
	(*print_int size;
	print_endline " ";*)
	(*Stack.iter (fun (s,a) -> print_string s; print_string " "; print_int a; print_endline " ") stack;
	print_endline "je suis ici";*)
	(*print_endline (string_of_int (Stack.length stack));
	print_endline (string_of_int size) ;*)
	gen_tree_of_stack (stack,size) with_prefix idprefix
	

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
		(*Array.iter (fun e -> print_endline (string_of_float e)) y;
		print_endline "";*)
		let rec try_gen (nb_try:int) (nb_smaller:int) (nb_bigger:int) : ((tree * int) option * int * int) =
			if nb_try > 0 then
				(match gen_tree g with_prefix idprefix sizemax y with
				| (Some tree,size) ->
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
				else failwith "Your trees are too big, change paramaters please")
		else None (* refined too much : could not generate a tree *)
	in
	gen epsilon1 epsilon2 0. 1. 1

