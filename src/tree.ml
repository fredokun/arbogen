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


type 'a t = Node of 'a * ('a t list)

(* temporary workaround *)
let annotate tree =
  let id = ref (-1) in
  let next () = incr id; !id in
  let rec aux (Node (x, children)) =
    Node ((x, next () |> string_of_int), List.map aux children)
  in
  aux tree

let rec indent_string = function
  | 0 -> ""
  | n -> "  " ^  indent_string (n-1)

let rec tree_out (show_type:bool) (show_id:bool) tree out =
  let label typ id =
    if show_type then
      if show_id then typ ^ ":" ^ id
      else typ
    else
    if show_id then id
    else ""
  in
  let Node ((typ, id), ts) = tree in
  output_string out (label typ id) ;
  Util.output_list
    out
    (fun (out:out_channel) t -> (tree_out show_type show_id t out))
    "[" "," "]" ts

let file_of_tree (show_type:bool) (show_id:bool) tree out =
  tree_out show_type show_id tree out ;
  output_string out "\n"

let attributes buf typ id show_type show_id =
  Buffer.add_string buf (if show_type then "type=\"" ^ typ ^ "\" " else "");
  Buffer.add_string buf (if show_id then "id=\"" ^ id ^ "\"" else "")

let xml_of_tree (show_type:bool) (show_id:bool) t =
  let buf = Buffer.create 1024 in
  let rec aux (Node ((typ, id), ts)) =
    Buffer.add_string buf "<node ";
    attributes buf typ id show_type show_id;
    Buffer.add_string buf ">";
    Util.string_of_list_buf aux buf "" "" "</node>" ts
  in
  Buffer.add_string buf "<?xml version=\"1.0\"?><tree>";
  aux t;
  Buffer.add_string buf "</tree>";
  buf

let indent_xml_of_tree (show_type:bool) (show_id:bool) t =
  let buf = Buffer.create 1024 in
  let rec tree level (Node ((typ, id), ts)) =
    Buffer.add_string buf (indent_string level);
    Buffer.add_string buf "<node ";
    attributes buf typ id show_type show_id;
    Buffer.add_string buf ">";
    forest (level+1) ts;
    Buffer.add_string buf "\n";
    Buffer.add_string buf (indent_string level);
    Buffer.add_string buf "</node>";
  and forest level = function
    | [] -> ()
    | [t] -> tree level t
    | t::f' ->
      tree level t;
      Buffer.add_string buf "\n";
      forest level f'
  in
  Buffer.add_string buf "<?xml version=\"1.0\"?>\n<tree>\n";
  tree 1 t;
  Buffer.add_string buf "\n</tree>\n";
  buf

let dot_of_tree (show_type:bool) (show_id:bool) (indent: bool) t =
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
  let rec nodes (Node ((typ, id), ts)) =
    Buffer.add_string buf "  ";
    Buffer.add_string buf id;
    Buffer.add_string buf (label typ id);
    Util.string_of_list_buf nodes buf "" "" "" ts;
  and edges level pred (Node ((_, id), ts)) =
    Buffer.add_string buf (if indent then (indent_string level) else "");
    Buffer.add_string buf pred;
    Buffer.add_string buf " -> ";
    Buffer.add_string buf id;
    Buffer.add_string buf ";\n";
    Util.string_of_list_buf (fun t -> edges (level+1) id t) buf "" "" "" ts;
  in
  Buffer.add_string buf "digraph {\n";
  nodes t;
  (match t with Node ((_,id),ts) ->
     Util.string_of_list_buf (fun t -> edges 1 id t) buf "" "" "" ts);
  Buffer.add_string buf "}\n";
  buf

let file_of_dot (show_type:bool) (show_id:bool) (indent: bool) tree out =
  let buf = dot_of_tree show_type show_id indent tree in
  Buffer.output_buffer out buf

let file_of_xml (show_type:bool) (show_id:bool) (indent: bool) tree out =
  let buf =
    if indent then
      indent_xml_of_tree show_type show_id tree
    else
      xml_of_tree show_type show_id tree
  in
  Buffer.output_buffer out buf
