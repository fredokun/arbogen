(** {2 The internal representation of grammar} *)

(** A grammar is an array of production rules.
    For performance reasons, the symbols of the grammars are integers:
    - symbol [i] is defined in [rules.(i)]
    - the high level name of symbol [i] is [names.(i)]

    For instance, the grammar of binary trees [B ::= 1 + z * B * B] may be
    represented like:
    - [rules = [| [(0, []); (1, [Elem 0; Elem 0])] |]]
    - [names = [| "B" |]]
*)
type t = {names: string array; rules: rule array}

(** A rule is a list of possible derivations *)
and rule = component list

(** A component [(n, elems)] is the product of an atom of size [n]
    and a possibly empty list of components *)
and component = int * elem list

(** An element is either of reference to a symbol or a sequence (Kleene star) *)
and elem =
  | Elem of int
  | Seq of int

(** {2 Some basic shorthands} *)

(** The only component of size 0 *)
val epsilon: component

(** {2 Evaluation using the symbolic method} *)

val eval_elem: values:float array -> elem -> float
val eval_component: z:float -> values:float array -> component -> float
val eval_rule: z:float -> values:float array -> rule -> float
val eval: ?dest:float array -> z:float -> values:float array -> t -> float array

(** {2 Conversion} *)

val of_parsetree: Frontend.ParseTree.t -> t

(** {2 Pretty printing} *)

(** Pretty printer for grammars *)
val pp: Format.formatter -> t -> unit
