type elem = SEQ of string | ELEM of string

type component =  int * elem list

type rule = string * component list

type grammar = rule list
