(*********************************************************
 * Arbogen-lib : fast uniform random generation of trees *
 *********************************************************
 * Module: Gen                                           *
 * -------                                               *
 * The Boltzmann random generator                        *
 * -------                                               *
 * (C) 2011, Xuming Zhan, Frederic Peschanski            *
 *           Antonine Genitrini under the                *
 *           GNU GPL v.3 licence (cf. LICENSE file)      *
 *********************************************************)

open Tree

open Util

open CombSys

open Grammar

open OracleSimple

let gra_gratab (g:grammar) : string array = 
	let l = List.length g in
	let r = Array.make l "name" in
	let e = l-1 in
	for i=0 to e do
		let (p1,_,_) = List.nth g i in
		r.(i) <- p1
	done;
	r


let gra_weightTab (g:grammar) : int array = 
	let l = List.length g in
	let r = Array.make l 1 in
	let e = l-1 in
	for i=0 to e do
		let (_,p2,_) = List.nth g i in
		r.(i) <- p2
	done;
	r

let strlist2cp (sl:string list) (w:int) (stab:string array):combprod =
	let l = List.length sl in
	let le = l - 1 in
	let wt = Array.make w Z in
	let thetl = Array.to_list wt in
	(*print_endline "tl";
	print_endline (string_of_int (List.length thetl));*)
	let res = Array.make l One in
	let thejstr = ref "string" in
	let indstr = ref 100 in
	for j = 0 to le do
		thejstr := List.nth sl j;
		indstr := findIndexstr stab !thejstr;
		res.(le-j) <- (Refe !indstr)
	done;
	let thehd = Array.to_list res in
	(*print_endline "hd";
	print_endline (string_of_int (List.length thehd));
	print_combprod (List.append thehd thetl);*)
	List.append thehd thetl
		
	
	
let rule2combeq (s:string) (w:int) (r:rule) (stab:string array) (wtab:int array):combeq =
	match r with
		Terminal -> 
			let ind = findIndexstr stab s in
			let wei = wtab.(ind) in
			if wei = 0 then
				[[One]]
			else
			begin
				let thecntab = Array.make wei Z in
				let thecn = Array.to_list thecntab in
				[thecn]	
			end
		|NonTerminal ll ->
			let l = List.length ll in
			(*print_endline (string_of_int l);*)
			let le = l - 1 in
			let restab = Array.make l [Z] in
			let inv = ref 1 in
			let sl = ref ["dgf";"dfs"] in
			for j = 0 to le do
				inv := le - j;
				sl := List.nth ll !inv;
				restab.(j) <- strlist2cp !sl w stab 
			done;
			let res = Array.to_list restab in
			(*print_endline (string_of_int (List.length res));*)
			res
	

		
let gra_toCombSys (g:grammar) : combsys = 

	let l = List.length g in
	let le = l - 1 in
	let res =  Array.make l [[One]] in
	let strtab = gra_gratab g in
	let wtab = gra_weightTab g in
	for i = 0 to le do
		res.(i) <- (rule2combeq (grastr g i) (graint g i) (grarule g i) strtab wtab)
	done;
	res

let getvalue (gtab:string array) (y:float array) (therule:string) : float =
	let r = ref 0.0 in
	let thel = Array.length gtab in
	let theend = thel - 1 in
	for i =0 to theend do
		if (String.compare therule (Array.get gtab i)) = 0 then 
			r :=  Array.get y i
 	done;
	!r
	 

let strgetvalue (gtab:string array) (y:float array) (sl:string list) : float =
	let r = ref 1.0 in
	let thel = List.length sl in
	let theend = thel - 1 in
	for i =0 to theend do
		let multipl = getvalue gtab y (List.nth sl i) in
		r := (!r) *. multipl
 	done;
	!r

let bernoulli (g:grammar) (y:float array) (therulestring:string) (sll:string list list) : string list= 
	let thelen = List.length sll in
	let gratab = gra_gratab g in
	let interval = Array.make thelen 0.0 in
	let divi = getvalue gratab y therulestring in
	let theend = thelen - 1 in
	for i = 0 to theend do
		interval.(i) <- strgetvalue gratab y (List.nth sll i);
		interval.(i) <- interval.(i) /. divi;
		if i > 0 then
			interval.(i) <- interval.(i-1) +. interval.(i);
	done;
	
	let rd = Random.float 1.0 in
	let ind = ref 0 in
	for j = 1 to theend do
		if rd > interval.(j-1) && rd <= interval.(j) then
			ind := j
	done;
	List.nth sll (!ind)
	
				
let rec gra_rulegen (g:grammar) (r:string) (y:float array):tree = 
	let therule = gra_findrule g r in
	match therule with
		Terminal -> tree_leaf r
		|NonTerminal sll -> 
			let thestrlist = bernoulli g y r sll in
			let thelen = List.length thestrlist in
			let theend = thelen - 1 in
			let theone = List.nth thestrlist 0 in
			let theter = gra_findTer g in
			
			if thelen = 1 && compareStrArr theone theter = true  
			then
			begin		
				tree_leaf (List.nth thestrlist 0)
			end			
			else
			begin
				let theN = Tree.Node("",[(Tree.Leaf "");(Tree.Leaf "")]) in			
				let thetreearray = Array.make thelen theN in
				for i = 0 to theend do
					thetreearray.(i) <- gra_rulegen g (List.nth thestrlist i) y;
				done;
				let thetreelist = Array.to_list thetreearray in
				tree_Node r thetreelist
			end
(*let thecombsys = gra_toCombSys g in *)

let generator 
    (g:grammar) 
    (sizemin:int) (sizemax:int) 
    (epsilon1:float) (epsilon1_factor:float)
    (epsilon2:float) (epsilon2_factor:float)
    (idprefix:string) 
    (max_try:int) (max_refine:int) : (tree*int) option =
  let sys = combsys_of_grammar g in
  let rec gen epsilon1 epsilon2 pmin pmax nb_refine =
    let (pmin',pmax',sing) = searchSingularity sys pmin pmax epsilon1 epsilon2 in
    let rec try_gen nb_try nb_smaller nb_bigger =
      if nb_try > 0 then
        (let (tree,size) = gen_tree g idprefix sing in
         if size<sizemin then
           try_gen (n-1) (nb_smaller+1) nb_bigger
         else if size>sizemax then
           try_gen (n-1) nb_smaller (nb_bigger+1)
         else (* ok, the tree has an acceptable size *)
           (Some ((tree,size),nb_smaller,nb_bigger)))
      else (* max number of tries *)
        (None,nb_smaller,nb_bigger) 
    in
    if nb_refine>0 then
      (let (ptree,nb_smaller,nb_larger) = try_gen max_try 0 0 in
       match ptree with
         | Some _ -> ptree (* ok, found a tree, return it with its size *)
         | None -> 
           if (float_of_int nb_smaller) /. (float_of_int (nb_smaller+nb_larger)) >= 0.8
           then (* if more than 80% of the trees are too small, then refine *)
             gen (epsilon1 *. epsilon1_factor) (epsilon2 *. epsilon2_factor) pmin' pmax' (nb_refine+1))
    else (* refined too much : could not generate a tree *)
      (None,0) 
