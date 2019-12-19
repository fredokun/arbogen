val countAll: Grammar.t -> int -> (int array array)

val count: (int array array) -> Grammar.t -> int Grammar.expression -> int -> int -> int -> int

val countSizeZero: (int array array) -> Grammar.t -> int Grammar.expression -> int -> unit

val countUnionProductZero: (int array array) -> Grammar.t -> int Grammar.expression -> int -> int
