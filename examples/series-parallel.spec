// Grammar for plane series-parallel graphs

Tree ::=   Serie + Parallel 
Serie ::=  Leaf * <z> + P * P * SEQ(P)
P ::= Parallel + Leaf * <z>
Parallel ::= Leaf * <z> +  S * S * SEQ(S)
S ::= Serie + Leaf * <z>
