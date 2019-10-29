type t = {
  randgen: string;
  rnd_state: Bytes.t;
  weighted_grammar: WeightedGrammar.t
}

(** Load a generation state from a file *)
val from_file: string -> t

(** Dump a generation state into a file *)
val to_file: string -> t -> unit
