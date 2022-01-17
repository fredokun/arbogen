(** {2 Pseudo-random number generators (PRNG)} *)

(** The internal state of a PRNG must be serialisable to be usable with Arbogen.
    This signature specifies serialisable types. *)
module type STATE = sig
  type t

  val to_bytes: t -> Bytes.t
  val from_bytes: Bytes.t -> t
end

(** This is the types of a PRNG to be used with Arbogen. *)
module type S = sig
  module State : STATE

  val name: string
  (** A unique name for the PRNG. *)

  val init: int -> unit
  (** Seed the random generator with an integer. *)

  val self_init: unit -> unit
  (** Let the generator self-seed itself. *)

  val int: int -> int
  (** [int bound] computes a random integer between [0] (inclusive) and [bound]
    (exclusive). *)

  val float: float -> float
  (** [float bound] computes a random float between [0.] (inclusive) and [bound]
   * (exclusive). *)

  val get_state: unit -> State.t
  (** Return the current state of the random generator. *)

  val set_state: State.t -> unit
  (** Change the current state of the random generator. *)
end

(** {3 Pre-defined PRNG} *)

module OcamlRandom: S = OcamlRandom
(** A thin wrapper around OCaml's {!Stdlib.Random} module *)

module Randnull: S = Randnull
(** A very bad PRNG with hardcoded values, for testing purposes only! *)

module Randu: S = Randu
(** The randu (https://en.wikipedia.org/wiki/RANDU) PRNG.
    This is a bad PRNG with respect to nowadays standards, don't use it in
    production. *)


(** {2 Classical random distributions} *)

(** The geometric distribution.
    If [0 < p <= 1], [geometric p] returns [n] with probability
    [p * (1-p)^n]. *)
let geometric (module R: S) =
  let rec gen acc p =
    if R.float 1. < p then acc
    else gen (acc + 1) p
  in
  fun p ->
    if p <= 0. || p > 1.
    then invalid_arg "Randtools.geometric: argument must be in (0; 1]"
    else gen 0 p

(* let geometric (module R: Sig.S) p = *)
(*   let rec gen r p_pow_i = *)
(*     let r = r -. p_pow_i in *)
(*     if r < 0. then 0 *)
(*     else 1 + gen r (p_pow_i *. p) *)
(*   in *)
(*   gen (R.float 1.) p *)
