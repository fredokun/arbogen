type elem = SEQ of string | ELEM of string

type component = int option * ((elem option) list)

type rule = string * (component list)

type grammar = rule list
