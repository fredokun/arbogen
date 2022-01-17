(** Boltzmann random sampling of tree-like structures *)

(** Arbogen provides efficient approximate size uniform samplers for structures
    that can be described by an unambiguous context-free grammar.
    Arbogen implements a generic Boltzmann sampler, as described in the paper
    ``Boltzmann Samplers for the Random Generation of Combinatorial Structures''
    by Duchon, Flajolet, Louchard and Schaeffer.

    The algorithm offers two guarantees:
    - two objects of the same size have the same probability of being drawn;
    - given two integers [size_min] and [size_max] the algorithm will generate
    an object of size larger or equal to [size_min] and less or equal to
    [size_max] in linear time (in its size), provided that [size_max - size_min]
    grows linearly with [size_min].

    To be continued...
    *)

module Frontend = Frontend
module Grammar = Grammar
module Boltzmann = Boltzmann

module Randtools = Randtools
module Tree = Tree
module Oracles = Oracles
