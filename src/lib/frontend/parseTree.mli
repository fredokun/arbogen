(** High-level grammar definition *)

(** {2 Grammar} *)

type t = rule list
(** Grammars are lists of rules. *)

and rule = string * product list
(** A rule binds a non-terminal symbol to a list of possible derivations.
  The rule [(symbol, [d1; d2; ...; dn])] reads, in mathematical notations:
  symbol :== d1 | d2 | ... |dn *)

and product = atomic list
(** In our setup, a rule derivation may only be a product of atomic
  expressions. This restriction may be dropped in the future. *)

and atomic =
  | Z of int
  | Elem of string
  | Seq of string
(** The atomic expression of the language are:
  - atoms [Z n] where [n] denotes the size of the atom
  - references to other rules [Elem name]
  - the sequence construction of analytic combinatorics [Seq name] *)

(** {2 Grammar completion} *)

val is_complete: t -> bool
(** Whether all the symbols occurring in the grammar are bound. *)

val completion: t -> t
(** Add a rule for each unbound symbol, interpreting it as an atom of size 0. *)

(** {2 Conversion to Grammars} *)

val to_grammar: t -> Grammar.t

(** {2 Pretty-printing} *)

val pp: Format.formatter -> t -> unit
val pp_rule: Format.formatter -> rule -> unit
val pp_product: Format.formatter -> product -> unit
val pp_atomic: Format.formatter -> atomic -> unit
