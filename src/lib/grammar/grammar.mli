(** The internal representation of grammars *)

(** A grammar is an array of production rules. For performance reasons, the
    non-terminals of the grammars are represented by integers: - symbol [i] is
    defined in [rules.(i)] - the high level name of symbol [i] is [names.(i)]

    For instance, the grammar of binary trees [B ::= 1 + z * B * B] may be
    encoded as follows:
    {[
      { rules = [|Union (Z 0, Product (Z 1, Product (Ref 0; Ref 0)))|]
      ; names = [|"B"|] }
    ]}

    As another example, the grammar of plane trees might be represented either
    using the sequence construction [T ::= z * Seq(T)]:
    {[
      { rules = [|Product (Z 1, Seq (Ref 0))|]
      ; names = [|"T"|] }
    ]}
    Or using a two-rules grammar to explicitely express the sequence
    [T ::= z * S and S ::= 1 + T * S]:
    {[
      { rules = [| Product (Z 1, Ref 1); Union (Z 0, Product (Ref 0, Ref 1)); |]
      ; names = \[|"T"; "S"|]; }
    ]} *)
type t = {names: string array; rules: int expression array}

(** The currently supported grammar expressions. *)
and 'ref expression =
  | Z of int
      (** The atom, potentially elevated to the power of some integer. This
          accounts for the size of the generated object. *)
  | Product of 'ref expression * 'ref expression  (** Cartesian product *)
  | Union of 'ref expression * 'ref expression
      (** Disjoint unions (alternative between two possible derivations) *)
  | Seq of 'ref expression
      (** Sequence of objects. This behaves as a Kleene start, that is [Seq e]
          behaves as [Union (Z 0, Product (e, Seq e)] *)
  | Ref of 'ref
      (** A non-terminal symbol. The type of the references is left abstract in
          order to allow both integer references ([Ref i] refers to the [i]th
          non-terminal of the grammar) and strings (for use in the parser). *)

(** {2 Pretty printing} *)

val pp : Format.formatter -> t -> unit
(** Pretty printer for grammars *)

val pp_expression :
     pp_ref:(Format.formatter -> 'ref -> unit)
  -> Format.formatter
  -> 'ref expression
  -> unit
(** Pretty printer for expressions *)
