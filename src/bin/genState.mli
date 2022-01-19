type t =
  { randgen: string
  ; rnd_state: Bytes.t
  ; weighted_grammar: Boltzmann.WeightedGrammar.t }

val from_file : string -> t
(** Load a generation state from a file *)

val to_file : string -> t -> unit
(** Dump a generation state into a file *)
