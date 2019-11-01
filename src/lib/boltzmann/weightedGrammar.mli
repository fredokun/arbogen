(** Grammars annotated with weight information *)

(** {2 Type definitions} *)

(** Analog of {!Grammar.t} with weight information *)
type t = {rules: expression array; names: string array}

(** Analog of {!Grammar.expression} with weight information. *)
and expression = {weight: float; desc: expression_desc}
and expression_desc =
  | Z of int
  | Product of expression * expression
  | Union of expression * expression
  | Seq of expression
  | Reference of int


(** {2 Grammar annotations} *)

(** Annotate an expression. *)
val of_expression:
  Oracles.Types.unlabelled
  -> int Grammar.expression
  -> expression

(** Annotate a grammar. *)
val of_grammar: Oracles.Types.unlabelled -> Grammar.t -> t

(** {2 Pretty printing} *)

(** Pretty print a weighted grammar *)
val pp: Format.formatter -> t -> unit
