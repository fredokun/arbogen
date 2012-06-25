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

open CombSys ;;

open OracleSimple;;

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

let tree = [("SEQ(Node)",[(1,["Node"])]) ; ("Node", [ (1,["Leaf"]) ; (1,["SEQ(Node)"]) ]) ] in
let (s,_) = string_of_combsys (combsys_of_grammar (completion tree)) in
print_endline s ;;

