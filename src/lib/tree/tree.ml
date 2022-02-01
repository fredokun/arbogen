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

type 'a t = Label of 'a * 'a t list | Tuple of 'a t list

(** {2 Iterators} *)

let fold ~label ~tuple =
  let rec dfs k = function
    | Label (l, children) ->
      dfs_forest (fun xs -> k (label l xs)) children
    | Tuple children ->
      dfs_forest (fun xs -> k (tuple xs)) children
  and dfs_forest k = function
    | [] ->
      k []
    | tree :: forest ->
      dfs (fun x -> dfs_forest (fun xs -> k (x :: xs)) forest) tree
  in
  dfs Fun.id

(** {2 Output functions} *)

(* temporary workaround *)
let annotate tree =
  let id = ref (-1) in
  let next () = incr id; !id in
  fold
    ~label:(fun l children -> Label ((l, next () |> string_of_int), children))
    ~tuple:(fun children -> Tuple children)
    tree

(* Some pretty printing utilities *)

let print_list ~print_elem ~print_op ~print_dl ~print_cl xs =
  let rec aux = function
    | [] ->
      ()
    | [x] ->
      print_elem x
    | x :: xs ->
      print_elem x; print_dl (); aux xs
  in
  print_op (); aux xs; print_cl ()

let string_of_list_buf print_elem buf op dl cl =
  print_list ~print_elem
    ~print_op:(fun () -> Buffer.add_string buf op)
    ~print_dl:(fun () -> Buffer.add_string buf dl)
    ~print_cl:(fun () -> Buffer.add_string buf cl)

let output_list out output_elem op dl cl =
  print_list
    ~print_elem:(fun x -> output_elem out x)
    ~print_op:(fun () -> output_string out op)
    ~print_dl:(fun () -> output_string out dl)
    ~print_cl:(fun () -> output_string out cl)

let add_indent =
  let s = ref (String.make 16 ' ') in
  let grow n =
    let len = String.length !s in
    s := String.make (max n (2 * (len + 1))) ' '
  in
  fun buf n ->
    let len = String.length !s in
    if 2 * n >= len then grow (2 * n);
    Buffer.add_substring buf !s 0 (2 * n)

(* .arb *)

let tree_out (show_type : bool) (show_id : bool) =
  let label typ id =
    if show_type then if show_id then typ ^ ":" ^ id else typ
    else if show_id then id
    else ""
  in
  let rec print_tree out = function
    | Label ((typ, id), ts) ->
      output_string out (label typ id);
      output_list out
        (fun (out : out_channel) t -> print_tree out t)
        "[" "," "]" ts
    | Tuple ts ->
      output_list out
        (fun (out : out_channel) t -> print_tree out t)
        "[" "," "]" ts
  in
  fun tree out -> print_tree out tree

(* .xml *)

let attributes buf typ id show_type show_id =
  Buffer.add_string buf (if show_type then " type=\"" ^ typ ^ "\"" else "");
  Buffer.add_string buf (if show_id then " id=\"" ^ id ^ "\"" else "")

let xml_of_tree (show_type : bool) (show_id : bool) t =
  let buf = Buffer.create 1024 in
  let rec aux = function
    | Label ((typ, id), ts) ->
      Buffer.add_string buf "<node";
      attributes buf typ id show_type show_id;
      Buffer.add_string buf ">";
      string_of_list_buf aux buf "" "" "</node>" ts
    | Tuple ts ->
      Buffer.add_string buf "<node>";
      string_of_list_buf aux buf "" "" "</node>" ts
  in
  Buffer.add_string buf "<?xml version=\"1.0\"?><tree>";
  aux t;
  Buffer.add_string buf "</tree>";
  buf

let indent_xml_of_tree (show_type : bool) (show_id : bool) t =
  let buf = Buffer.create 1024 in
  let rec tree level = function
    | Label ((typ, id), ts) ->
      add_indent buf level;
      Buffer.add_string buf "<node";
      attributes buf typ id show_type show_id;
      Buffer.add_string buf ">\n";
      forest (level + 1) ts;
      add_indent buf level;
      Buffer.add_string buf "</node>\n"
    | Tuple ts ->
      add_indent buf level;
      Buffer.add_string buf "<node>";
      forest (level + 1) ts;
      add_indent buf level;
      Buffer.add_string buf "</node>\n"
  and forest level = function
    | [] ->
      ()
    | [t] ->
      tree level t
    | t :: f' ->
      tree level t; forest level f'
  in
  Buffer.add_string buf "<?xml version=\"1.0\"?>\n<tree>\n";
  tree 1 t;
  Buffer.add_string buf "</tree>\n";
  buf

(* .dot *)

let dot_of_tree (show_type : bool) (show_id : bool) (indent : bool) t =
  let label typ id =
    let aux typ id =
      if show_type then if show_id then typ ^ ":" ^ id else typ
      else if show_id then id
      else ""
    in
    let l = aux typ id in
    if l = "" then " [shape=point];\n" else " [label=\"" ^ l ^ "\"];\n"
  in
  let buf = Buffer.create 1024 in
  let rec nodes tid = function
    | Label ((typ, id), ts) ->
      Buffer.add_string buf "  ";
      Buffer.add_string buf id;
      Buffer.add_string buf (label typ id);
      nodes_forest tid ts
    | Tuple ts ->
      Buffer.add_string buf "  ";
      Buffer.add_string buf ("t" ^ string_of_int tid);
      Buffer.add_char buf '\n';
      nodes_forest (tid + 1) ts
  and nodes_forest tid = function
    | [] ->
      tid
    | t :: ts ->
      let tid = nodes tid t in
      nodes_forest tid ts
  in
  let rec edges level tid parent = function
    | Label ((_, id), ts) ->
      if indent then add_indent buf level;
      Buffer.add_string buf parent;
      Buffer.add_string buf " -> ";
      Buffer.add_string buf id;
      Buffer.add_string buf ";\n";
      edges_forest (level + 1) tid id ts
    | Tuple ts ->
      let id = "t" ^ string_of_int tid in
      if indent then add_indent buf level;
      Buffer.add_string buf parent;
      Buffer.add_string buf " -> ";
      Buffer.add_string buf id;
      Buffer.add_string buf ";\n";
      edges_forest (level + 1) (tid + 1) id ts
  and edges_forest level tid parent = function
    | [] ->
      tid
    | t :: ts ->
      let tid = edges level tid parent t in
      edges_forest level tid parent ts
  in
  Buffer.add_string buf "digraph {\n";
  let _ = nodes 0 t in
  let _ =
    match t with
    | Label ((_, id), ts) ->
      let level = 1 in
      let parent = id in
      let tid = 0 in
      edges_forest level tid parent ts
    | Tuple ts ->
      let level = 1 in
      let parent = "t0" in
      let tid = 1 in
      edges_forest level tid parent ts
  in
  Buffer.add_string buf "}\n";
  buf

(* public functions *)

let output_arb ~show_type ~show_id ~indent out tree =
  if indent then
    Format.printf "Warning: -indent not supported for the arb format@.";
  tree_out show_type show_id tree out;
  output_string out "\n"

let output_dot ~show_type ~show_id ~indent out tree =
  let buf = dot_of_tree show_type show_id indent tree in
  Buffer.output_buffer out buf

let output_xml ~show_type ~show_id ~indent out tree =
  let buf =
    if indent then indent_xml_of_tree show_type show_id tree
    else xml_of_tree show_type show_id tree
  in
  Buffer.output_buffer out buf
