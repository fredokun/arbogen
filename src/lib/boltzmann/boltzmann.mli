(** Boltzmann generation for tree-like structures. *)

(** {2 Pre-processing phase} *)

module WeightedGrammar = WeightedGrammar
module Oracle = Oracle

(** {2 Rejection sampling in a size window (high-level interface)} *)

val search_seed :
     (module Randtools.S with type State.t = 'a)
  -> size_min:int
  -> size_max:int
  -> ?max_try:int
  -> WeightedGrammar.t
  -> (int * 'a) option
(** Search for a tree in a specific size window by rejection sampling.
    Only the size of the tree is computed during the generation.
    This function returns the size of the found tree and the state of the PRNG
    just before generating this tree. *)

val generator :
     Grammar.t
  -> Oracle.t
  -> (module Randtools.S)
  -> size_min:int
  -> size_max:int
  -> max_try:int
  -> (string Tree.t * int) option
(** [generator grammar oracle rng ~size_max ~size_min ~max_try] generates trees
    until it finds one of size at least [size_min] and at most [size_max]. After
    [max_try] unsuccessful attempts, it returns [None]. *)

(** {2 Free Bolzmann generators (low-level interface)} *)

(** The "free" keyword means that no rejection is performed: these generators
    generate one object following the Boltzmann distribution. *)

val free_size : (module Randtools.S) -> size_max:int -> WeightedGrammar.t -> int
(** [free_size rng size_max rules] simulates the generation of a tree given the
    weighted grammar [rules] (starting by the first symbol) but only computes
    its size. The generation aborts early if the size goes beyond [size_max]. *)

val free_gen :
  (module Randtools.S) -> WeightedGrammar.t -> string -> string Tree.t * int
(** Generate a tree and its size, given weigthed grammar and the desired
    non-terminal. *)
