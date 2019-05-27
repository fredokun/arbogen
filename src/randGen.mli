(** A PRNG must implement this interface to be used for Boltzmann generation *)
module type Sig = sig
  val name: string
  val init: int -> unit
  val self_init: unit -> unit
  val int: int -> int
  val float: float -> float
  val get_state: unit -> Random.State.t
  val set_state: Random.State.t -> unit
end

(** OCaml's Random module *)
module OcamlRandom: Sig

(** The Randu linear congruential generator *)
module Randu: Sig

(** A very stupid generator that cycles through a list of predefined values.
    For testing purpose only *)
module RandNull: Sig

(** The list of all predefined PRNGs *)
val all_rand_gens: (string * (module Sig)) list

(** A shorthand for getting a PRNG based on its name *)
val get: string -> (module Sig)
