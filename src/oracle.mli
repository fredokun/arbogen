(** {2 Unlabelled oracles} *)

(** For unlabelled generation, it is sufficient to only know the values of the
    generating functions at [z] *)
type unlabelled = {z: float; values: float array}

(** {2 Grammar evaluation} *)

(** Evaluate an element with respect to an oracle *)
val eval_elem: unlabelled -> Grammar.elem -> float

(** Evaluate a component with respect to an oracle *)
val eval_component: unlabelled -> Grammar.component -> float

(** Evaluate a rule with respect to an oracle *)
val eval_rule: unlabelled -> Grammar.rule -> float

(** Evaluate a grammar with respect to an oracle.
    If a destination array [dest] is supplied, the result is stored in [dest]
    rather than allocating a new array. *)
val eval: ?dest:float array -> unlabelled -> Grammar.t -> float array
