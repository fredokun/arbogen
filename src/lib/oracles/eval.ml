open Grammar
open Types


let elem oracle = function
  | Elem i -> oracle.values.(i)
  | Seq i -> 1. /. (1. -. oracle.values.(i))

let component oracle (n, elems) =
  List.fold_left
    (fun product e -> product *. elem oracle e)
    (oracle.z ** (float_of_int n))
    elems

let rule oracle components =
  List.fold_left
    (fun sum comp -> sum +. component oracle comp)
    0.
    components

let grammar_inplace dest oracle grammar =
  Array.iteri (fun i r -> dest.(i) <- rule oracle r) grammar.rules

let grammar oracle grammar =
  let dest = Array.create_float (Array.length grammar.rules) in
  grammar_inplace dest oracle grammar;
  dest
