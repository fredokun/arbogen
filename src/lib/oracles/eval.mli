(** Evaluation of grammar elements with respected to oracles. *)

val grammar: Types.unlabelled -> Grammar.t -> float array
(** Evaluate each rule of the grammar. *)

val grammar_inplace: float array -> Types.unlabelled -> Grammar.t -> unit
(** Same as [!grammar] but the result of the evaluation is stored in the array
  passed as first argument. *)

val expression: Types.unlabelled -> int Grammar.expression -> float
(** Evaluate an expression. *)
