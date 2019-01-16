(*********************************************************
 * Arbogen-lib : fast uniform random generation of trees *
 *********************************************************
 * Module: Tree                                          *
 * -------                                               *
 * Internal representation of trees and export tools     *
 * -------                                               *
 * (C) 2011, Xuming Zhan, Frederic Peschanski            *
 *           Antonine Genitrini, Matthieu Dien           *
 *           Marwan Ghanem                               *
 *           under the                                   *
 *           GNU GPL v.3 licence (cf. LICENSE file)      *
 *********************************************************)

open Util

type tree =
| Leaf of string * string     (* node type, node id *)
| Node of string * string * (tree ref list)   (* node type, node id, children *)

let rec indent_string = function
  | 0 -> ""
  | n -> "  " ^  indent_string (n-1)

let rec tree_out (show_type:bool) (show_id:bool) (tree:tree) out =
  let label typ id =
    if show_type then
      if show_id then typ ^ ":" ^ id
      else typ
    else
      if show_id then id
      else ""
  in
  match tree with
  | Leaf(typ,id) -> output_string out (label typ id)
  | Node(typ,id,ts) ->
    output_string out (label typ id) ;
    output_list
      out
      (fun (out:out_channel) t -> (tree_out show_type show_id !t out))
      "[" "," "]" ts
;;

let file_of_tree (show_type:bool) (show_id:bool) (tree:tree) out =
  tree_out show_type show_id tree out ;
  output_string out "\n";
  close_out out

let attributes buf typ id show_type show_id =
  Buffer.add_string buf (if show_type then "type=\"" ^ typ ^ "\" " else "");
  Buffer.add_string buf (if show_id then "id=\"" ^ id ^ "\"" else "")

let xml_of_tree (show_type:bool) (show_id:bool) (t:tree) =
  let buf = Buffer.create 1024 in
  let rec aux = fun x ->
    match !x with
    | Leaf(typ,id) ->
       begin
         Buffer.add_string buf "<leaf ";
         attributes buf typ id show_type show_id;
         Buffer.add_string buf "/>"
       end         
    | Node(typ,id,ts) ->
       begin
         Buffer.add_string buf "<node ";
         attributes buf typ id show_type show_id;
         Buffer.add_string buf ">";
         string_of_list_buf aux buf "" "" "</node>" ts
       end
  in
  Buffer.add_string buf "<?xml version=\"1.0\"?><tree>";
  aux (ref t);
  Buffer.add_string buf "</tree>";
  buf

let indent_xml_of_tree (show_type:bool) (show_id:bool) (t:tree) =
  let buf = Buffer.create 1024 in
  let rec tree level t = match t with
    | Leaf(typ,id) ->
       begin
         Buffer.add_string buf (indent_string level);
         Buffer.add_string buf "<leaf ";
         attributes buf typ id show_type show_id;
         Buffer.add_string buf "/>";
       end
    | Node(typ,id,ts) ->
       begin
         Buffer.add_string buf (indent_string level);
         Buffer.add_string buf "<node ";
         attributes buf typ id show_type show_id;
         Buffer.add_string buf ">";
         forest (level+1) ts;
         Buffer.add_string buf "\n";
         Buffer.add_string buf (indent_string level);
         Buffer.add_string buf "</node>";
       end
  and forest level f = match f with
    | [] -> ()
    | [t] -> tree level !t
    | t::f' ->
       begin
         tree level !t;
         Buffer.add_string buf "\n";
         forest level f'
       end
  in
  Buffer.add_string buf "<?xml version=\"1.0\"?>\n<tree>\n";
  tree 1 t;
  Buffer.add_string buf "\n</tree>\n";
  buf

let dot_of_tree (show_type:bool) (show_id:bool) (indent: bool) (t:tree) =
  let label typ id =
    let aux typ id =
    if show_type then
      if show_id then typ ^ ":" ^ id
      else typ
    else
      if show_id then id
      else ""
    in
    let l = aux typ id in
    if l = "" then
      " [shape=point];\n"
    else
      " [label=\"" ^ l ^ "\"];\n"
  in
  let buf = Buffer.create 1024 in
  let rec nodes = fun x ->
    match !x with 
    | Leaf(typ,id) ->
       begin
        Buffer.add_string buf "  ";
        Buffer.add_string buf id;
        Buffer.add_string buf (label typ id)
      end
    | Node(typ,id,ts) ->
       begin
         Buffer.add_string buf "  ";
         Buffer.add_string buf id;
         Buffer.add_string buf (label typ id);
         string_of_list_buf nodes buf "" "" "" ts;
       end
  and edges level pred t = match !t with
    | Leaf(_,id) ->
       begin
         Buffer.add_string buf (if indent then (indent_string level) else "");
         Buffer.add_string buf pred;
         Buffer.add_string buf " -> ";
         Buffer.add_string buf id;
         Buffer.add_string buf ";\n";
       end
    | Node(_,id,ts) ->
       begin
         Buffer.add_string buf (if indent then (indent_string level) else "");
         Buffer.add_string buf pred;
         Buffer.add_string buf " -> ";
         Buffer.add_string buf id;
         Buffer.add_string buf ";\n";
         string_of_list_buf (fun t -> edges (level+1) id t) buf "" "" "" ts;
       end
  in
  Buffer.add_string buf "digraph {\n";
  (nodes (ref t));
  (match t with
   | Leaf(_,_) -> ()
   | Node(_,id,ts) -> (string_of_list_buf (fun t -> edges 1 id t) buf "" "" "" ts));
  Buffer.add_string buf "}\n";
  buf

let file_of_dot (show_type:bool) (show_id:bool) (indent: bool) (tree:tree) out =
  let buf = dot_of_tree show_type show_id indent tree in
  Buffer.output_buffer out buf;
  close_out out

let file_of_xml (show_type:bool) (show_id:bool) (indent: bool) (tree:tree) out =
  let buf = 
    if indent then
      indent_xml_of_tree show_type show_id tree
    else
      xml_of_tree show_type show_id tree
  in Buffer.output_buffer out buf;
  close_out out;
