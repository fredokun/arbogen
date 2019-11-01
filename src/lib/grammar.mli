(** {2 The internal representation of grammar} *)

(** A grammar is an array of production rules.
  For performance reasons, the symbols of the grammars are integers:
  - symbol [i] is defined in [rules.(i)]
  - the high level name of symbol [i] is [names.(i)]

  For instance, the grammar of binary trees [B ::= 1 + z * B * B] may be
  represented like:
  - [rules = [| Union (Z 0, Product (Z 1, Product (Reference 0; Reference 0))) |]]
  - [names = [| "B" |]]
*)
type t = {names: string array; rules: int expression array}

(** The supported expressions are:
  - atoms, potentially elevated to the power of some integer
  - (Cartesian) products
  - disjoint unions (alternative between two possible derivations)
  - sequence (or Kleene star) of a sub-expression
  - a reference to a non-terminal
  The type of the references is left abstract in order to allow both integer
  references (like in [!t]) and string references (in the parser). *)
and 'ref expression =
  | Z of int
  | Product of 'ref expression * 'ref expression
  | Union of 'ref expression * 'ref expression
  | Seq of 'ref expression
  | Reference of 'ref

(** {2 Pretty printing} *)

(** Pretty printer for grammars *)
val pp: Format.formatter -> t -> unit

(** Pretty printer for expressions *)
val pp_expression:
  pp_ref:(Format.formatter -> 'ref -> unit)
  -> Format.formatter
  -> 'ref expression
  -> unit
