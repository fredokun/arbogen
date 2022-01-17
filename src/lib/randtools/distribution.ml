let geometric (module R: Sig.S) p =
  let rec gen acc =
    if R.float 1. < p then acc
    else gen (acc + 1)
  in
  gen 0

(* let geometric (module R: Sig.S) p = *)
(*   let rec gen r p_pow_i = *)
(*     let r = r -. p_pow_i in *)
(*     if r < 0. then 0 *)
(*     else 1 + gen r (p_pow_i *. p) *)
(*   in *)
(*   gen (R.float 1.) (1. -. p) *)
