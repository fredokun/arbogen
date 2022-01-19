(** Grammars annotated with weight information *)

(** {2 Type definitions} *)

(** Analog of {!Grammar.t} with weight information *)
type t = {rules: expression array; names: string array}

(** Analog of {!Grammar.expression} with weight information. *)
and expression =
  | Z of int
  | Product of expression * expression
  | Union of float * expression * expression
  | Seq of float * expression
  | Ref of int

(** {2 Grammar annotations} *)

val of_expression : Oracle.t -> int Grammar.expression -> expression
(** Annotate an expression. *)

val of_grammar : Oracle.t -> Grammar.t -> t
(** Annotate a grammar. *)

(** {2 Pretty printing} *)

val pp : Format.formatter -> t -> unit
(** Pretty print a weighted grammar *)
