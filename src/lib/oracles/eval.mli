(** Evaluation of grammar elements with respected to oracles. *)

val grammar: Types.unlabelled -> Grammar.t -> float array
(** Evaluate each rule of the grammar. *)

val grammar_inplace: float array -> Types.unlabelled -> Grammar.t -> unit
(** Same as [!grammar] but the result of the evaluation is stored in the array
 * passed as first argument. *)

val rule: Types.unlabelled -> Grammar.rule -> float
(** Evaluate a rule. *)

val component: Types.unlabelled -> Grammar.component -> float
(** Evaluate a grammar component. *)

val elem: Types.unlabelled -> Grammar.elem -> float
(** Evaluate an atomic elements of the grammar. *)
