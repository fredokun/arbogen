open Grammar
open Types


let expression oracle =
  let rec aux = function
    | Z n -> oracle.z ** (float_of_int n)
    | Product (e1, e2) -> (aux e1) *. (aux e2)
    | Union (e1, e2) -> (aux e1) +. (aux e2)
    | Seq e -> 1. /. (1. -. aux e)
    | Reference i -> oracle.values.(i)
  in
  aux

let grammar_inplace dest oracle grammar =
  Array.iteri (fun i r -> dest.(i) <- expression oracle r) grammar.rules

let grammar oracle grammar =
  let dest = Array.create_float (Array.length grammar.rules) in
  grammar_inplace dest oracle grammar;
  dest
