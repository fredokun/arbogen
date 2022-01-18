(** Boltzmann random sampling of tree-like structures *)

(** Arbogen provides efficient approximate size uniform samplers for structures
    that can be described by an unambiguous context-free grammar. Arbogen
    implements a generic Boltzmann sampler, as described in the paper
    ``Boltzmann Samplers for the Random Generation of Combinatorial Structures''
    by Duchon, Flajolet, Louchard and Schaeffer.

    The algorithm offers two guarantees: - two objects of the same size have the
    same probability of being drawn; - given two integers [size_min] and
    [size_max] the algorithm will generate an object of size larger or equal to
    [size_min] and less or equal to [size_max] in linear time (in its size),
    provided that [size_max - size_min] grows linearly with [size_min].

    The process of Boltzmann sampling consists in: 1. specifying the objects to
    be generated; 2. running a pre-processing step which computes some constants
    used in the generation; 3. generating objects in a size window using
    rejection sampling. At the moment, the objects produced by Arbogen all
    belong to the {!Tree.t} type. *)

(** {2 Combinatorial specifications} *)

(** They are two ways to specify the objects to be generated.

    When using Arbogen as a library, the simpler way is to write a grammar using
    the {!Grammar} module. Alternatively, one may describe the grammar in a
    .spec file (several examples are given in the [examples/] folder of the git
    repository). In that case, one must use the functions provided by the
    {!Frontend} to parse the file and obtain the corresponding grammar.

    In both cases, it is important to ensure that there exists a finite number
    of objects of each size. The size of an object is defined as the number of
    atoms ([Z n] in the grammar and [<z>] in the .spec language) occurring in a
    derivation. For instance, in the following grammar describing binary trees,
    the size of a tree corresponds to its number of internal nodes:

    - in the .spec language: [B ::= <1> + <z> * B * B] - as a {!Grammar.t}
    value:
    {[
      { names = [|"B"|]
      ; rules = [| Union (Z 0, Product (Z 1, Product (Ref 0, Ref 0))) |] }
    ]} *)

module Frontend = Frontend
module Grammar = Grammar

(** {2 Pre-processing} *)

(** The preprocessing of a grammar is handled by the {!Boltzmann.Oracle} and
    {!WeightedGrammar} modules.
    It consists in computing some weights (the values of the generating
    functions of the combinatorial classes at play) an annotating the grammar
    with theses weights.
    The current (only) implementation of this mechanism is available under the
    {!Boltzmann.Oracle.Naive} name.

    Obtaining an annotated grammar is achieved as follows:
    {[
      let open Boltzmann in
      let oracle = Oracle.Naive.make grammar in
      WeightedGrammar.of_grammar oracle grammar
    ]}

    See the {!Boltzmann.Oracle} documentation for more options.
 *)

(** {2 Random generation} *)

(** TODO: explain *)

module Boltzmann = Boltzmann
module Randtools = Randtools
module Tree = Tree
