(** Boltzmann generation of tree structures. *)

(** {2 Free Bolzmann generators (low-level interface)} *)

(** The "free" keyword means that no rejection is performed: these generators
 * generate one object following the Boltzmann distribution. *)

val free_size:
  (module Randtools.Sig.S)
  -> int
  -> WeightedGrammar.rule array
  -> int
(** [free_size rng size_max rules] simulates the generation of a tree following
 * the weighted grammar [rules] (starting by the first symbol) but only
 * generate its size. The generation stop early if the size goes beyond
 * [size_max]. *)

val free_gen:
  (module Randtools.Sig.S)
  -> WeightedGrammar.t
  -> string Tree.t * int
(** Generate a tree and its size, following the weigthed grammar given as
 * argument. *)

(** {2 Generation in a size window (high-level interface)} *)

val generator:
  Grammar.t
  -> Oracles.Types.unlabelled
  -> (module Randtools.Sig.S)
  -> size_min:int
  -> size_max:int
  -> max_try:int
  -> (string Tree.t * int * GenState.t) option
(** [generator grammar oracle rng ~size_max ~size_min ~max_try] generates trees
 * until it finds one of size at least [size_min] and at most [size_max]. After
 * [max_try] unsuccessful attempts, it returns [None]. *)
