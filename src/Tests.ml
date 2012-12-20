(*********************************************************
 * Arbogen-lib : fast uniform random generation of trees *
 *********************************************************
 * Module: Tests                                         *
 * -------                                               *
 * Generator tests (can be run in the top-level)         *
 * -------                                               *
 * (C) 2011, Xuming Zhan, Frederic Peschanski            *
 *           Antonine Genitrini, Matthieu Dien           *
 *           under the                                   *
 *           GNU GPL v.3 licence (cf. LICENSE file)      *
 *********************************************************)

open Tree;;

(* open CombSys ;; *)

(* open OracleSimple;; *)

open Util;;

open Gen;;
open Grammar;;


(*let tbtree = [("BinNode",[(1,["Leaf"]);(2,["TriNode";"TriNode"])]);
("TriNode",[(1,["Leaf"]);(0,["BinNode";"BinNode";"BinNode"])])] in
match generator tbtree true 0 80 150000 0.0001 0.1 0.000001 0.1 false "" 1000 0.8 8 with
|None -> failwith "a priori ça marche pas"
|Some(tree,_) -> print_endline (*(dot_of_tree true tree)*) (string_of_tree tree) ;;*)


(*let bintree = [ ("BinNode", [ (1,["Leaf"]) ; (1,["BinNode";"BinNode"]) ]) ] in
match generator bintree true 0 1 2000 0.001 0.1 0.00001 0.1 false "" 100 0.8 6 with
|None -> failwith "a priori ça marche pas"
|Some(tree,size) -> print_endline (string_of_int size) ; print_endline (dot_of_tree true tree) (*(string_of_tree tree)*) ;;*)

(*let bintree = [ ("TriNode", [ (1,["Leaf"]) ; (1,["TriNode";"TriNode";"TriNode"]) ]) ] in
match generator bintree true 0 100 150 0.001 0.1 0.0001 0.1 false "" 300 0.8 6 with
|None -> failwith "a priori ça marche pas"
|Some(tree,_) -> (*print_endline (string_of_int size) ;*) print_endline (dot_of_tree true tree) (*(string_of_tree tree)*) ;;*)

(*let (tree:(Elem.t * (int * Elem.t list) list) list) = [ (ELEM("T"),[(1,[SEQ("T")]);(1,[ELEM("Leaf")])]);(SEQ("T"),[(0,[ELEM("T")])])] in
let (s,_) = string_of_combsys (combsys_of_grammar (completion tree)) in
print_endline s ;;*)

let (plane_tree:grammar) = [ ("T",[(1,[SEQ("T")])])]
in
(*let bin_tree = [("T",[(1,[ELEM("T");ELEM("T")]);(1,[ELEM("Leaf")])])] in*)
(*List.iter (fun x -> print_endline x) (leafs_of_grammar plane_tree);
let (s,_) = string_of_combsys (combsys_of_grammar (completion plane_tree)) in
print_endline s ;
let sys = combsys_of_grammar (completion plane_tree) in
let (zmin,_,_) = searchSingularity sys 0. 1. 0.001 0.0001 1. in
print_endline (string_of_float zmin) ;;*)
(*let bintree = [ (ELEM("BinNode"), [ (1,[ELEM("Leaf")]) ; (1,[ELEM("BinNode");ELEM("BinNode")]) ]) ]
in*)
match generator plane_tree true 0 100000 1500000 0.1 0.1 0.1 0.1 false "" 100 0.8 10 with
|None -> failwith "Change your parameters"
|Some(tree,_) -> (*print_endline (string_of_int size) ;*) print_endline (dot_of_tree true tree) (*(string_of_tree tree)*) ;;
