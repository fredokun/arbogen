(** A PRNG must implement this interface to be used for Boltzmann generation. *)

(** The internal state of the RNG must be serializable. *)
module type STATE = sig
  type t

  val to_bytes: t -> Bytes.t
  val from_bytes: Bytes.t -> t
end

module type S = sig
  module State : STATE

  val name: string
  (** A unique name for PRNG. *)
  (* XXX. I don't like this at all. *)

  val init: int -> unit
  (** Seed the random generator with an integer. *)

  val self_init: unit -> unit
  (** Let the generator self-seed itself. *)

  val int: int -> int
  (** [int bound] computes a random integer between [0] (inclusive) and [bound]
   * (exclusive). *)

  val float: float -> float
  (** [float bound] computes a random float between [0.] (inclusive) and [bound]
   * (exclusive). *)

  val get_state: unit -> State.t
  (** Return the current state of the random generator. *)

  val set_state: State.t -> unit
  (** Change the current state of the random generator. *)
end
