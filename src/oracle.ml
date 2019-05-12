(** {2 Unlabelled oracles} *)

type unlabelled = {z: float; values: float array}

let eval_elem oracle = function
  | Grammar.Elem i -> oracle.values.(i)
  | Grammar.Seq i -> 1. /. (1. -. oracle.values.(i))

let eval_component oracle (atoms, factors) =
  List.fold_left
    (fun x elem -> x *. eval_elem oracle elem)
    (oracle.z ** float_of_int atoms)
    factors

let eval_rule oracle terms =
  List.fold_left
    (fun x component -> x +. eval_component oracle component)
    0.
    terms

let eval oracle grammar =
  let Grammar.{rules; _} = grammar in
  Array.map (eval_rule oracle) rules
