(** High-level grammar definition *)

(** {2 Grammar} *)

(** Grammars are lists of rules. *)
type t = rule list

(** A rule binds a non-terminal symbol to an expression describing how to derive
    this symbol. *)
and rule = string * string Grammar.expression

(** {2 Grammar completion} *)

val is_complete : t -> bool
(** Whether all the symbols occurring in the grammar are bound. *)

val completion : t -> t
(** Add a rule for each unbound symbol, interpreting it as an atom of size 0. *)

(** {2 Conversion to Grammars} *)

val to_grammar : t -> Grammar.t

(** {2 Pretty-printing} *)

val pp : Format.formatter -> t -> unit
