(** Grammars annotated with weight information *)

(** {2 Type definitions} *)

(** Analog to {!Grammar.elem} *)
type elem = Elem of int | Seq of int

(** Analog to {!Grammar.component} with weight information *)
type component = {weight: float; atoms: int; elems: elem list}

(** Analog to {!Grammar.rule} with weight information *)
type rule = {weight: float; choices: component list}

(** Analog to {!Grammar.t} with weight information *)
type t = {rules: rule array; names: string array}


(** {2 Grammar annotations} *)

(** Convert a grammar element into an element *)
val of_elem: Grammar.elem -> elem

(** Annotate a grammar component *)
val of_component: Oracles.Types.unlabelled -> Grammar.component -> component

(** Annotate a grammar rule *)
val of_rule: Oracles.Types.unlabelled -> Grammar.rule -> rule

(** Annotate a grammar *)
val of_grammar: Oracles.Types.unlabelled -> Grammar.t -> t

(** {2 Pretty printing} *)

(** Pretty print a weighted grammar *)
val pp: Format.formatter -> t -> unit
