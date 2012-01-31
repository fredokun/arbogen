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

type tree = 
    Leaf of string * string     (* node type, node id *)
  | Node of string * string * (tree list)   (* node type, node id, children *)

let rec string_of_tree = function
  | Leaf(typ,id) -> "Leaf{" ^ typ ^ "," ^ id ^ "}"
  | Node(typ,id,ts) -> "Node{" ^ typ ^ "," ^ id ^ "}" ^ (string_of_list string_of_tree "[" "," "]" ts)

let rec indent_string = function
  | 0 -> ""
  | n -> "  " ^  indent_string (n-1)

let indent_string_of_tree t =
  let rec tree level t = match t with
    | Leaf(typ,id) -> (indent_string level) ^ "Leaf{" ^ typ ^ "," ^ id ^ "}"
    | Node(typ,id,ts) -> 
      (indent_string level) ^ "Node{" ^ typ ^ "," ^ id ^ "}[\n" ^ (forest (level+1) ts) ^ "]"
  and forest level f = match f with
    | [] -> ""
    | [t] -> tree level t
    | t::f' -> (tree level t) ^ "\n" ^ (forest level f')
  in tree 0 t     

let xml_of_tree t = 
  let rec aux = function
    | Leaf(typ,id) -> "<leaf type=\"" ^ typ ^ "\" id=\"" ^ id ^ "\"/>"
    | Node(typ,id,ts) -> "<node type=\"" ^ typ ^ "\" id=\"" ^ id ^ "\">" ^ (string_of_list aux "" "" "</node>" ts)
  in "<?xml version=\"1.0\"?><tree>" ^ (aux t) ^ "</tree>" 

let indent_xml_of_tree t =
  let rec tree level t = match t with
    | Leaf(typ,id) -> (indent_string level) ^ "<leaf type=\"" ^ typ ^ "\" id=\"" ^ id ^ "\"/>"
    | Node(typ,id,ts) -> 
      (indent_string level) ^ "<node type=\"" ^ typ ^ "\" id=\"" ^ id ^ "\">\n" ^ (forest (level+1) ts) ^ "\n" ^ (indent_string level) ^ "</node>"
  and forest level f = match f with
    | [] -> ""
    | [t] -> tree level t
    | t::f' -> (tree level t) ^ "\n" ^ (forest level f')
  in "<?xml version=\"1.0\"?>\n<tree>\n" ^ (tree 1 t) ^ "\n</tree>\n"     

let dot_of_tree show_type t =
  let rec nodes = function
    | Leaf(typ,id) -> "  " ^ id ^ (if show_type then (" [label=\"" ^ typ ^ "\"];\n") else " [shape=point];\n")
    | Node(typ,id,ts) -> 
      "  " ^ id ^ (if show_type then (" [label=\"" ^ typ ^ "\"];\n") else " [shape=point];\n")
      ^ (string_of_list nodes "" "" "" ts)
  and edges level pred t = match t with
    | Leaf(_,id) -> (indent_string level) ^ pred ^ " -> " ^ id ^ ";\n"
    | Node(_,id,ts) -> (indent_string level) ^ pred ^ " -> " ^ id ^ ";\n" ^ (string_of_list (fun t -> edges (level+1) id t) "" "" "" ts)
  in
  "digraph {\n"
  ^ nodes t
  ^ (match t with
    | Leaf(_,_) -> ""
    | Node(_,id,ts) -> (string_of_list (fun t -> edges 1 id t) "" "" "" ts))
  ^ "}\n"

