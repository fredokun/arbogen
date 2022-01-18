type t =
  {randgen: string; rnd_state: Bytes.t; weighted_grammar: WeightedGrammar.t}

let from_file filename =
  let ic = open_in filename in
  try (input_value ic : t) with err -> close_in_noerr ic; raise err

let to_file filename state =
  let oc = open_out filename in
  try output_value oc state with err -> close_out_noerr oc; raise err
