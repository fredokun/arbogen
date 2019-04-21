// grammar for semantic trees of concurrent process
// constructed with shuffle and non-determinism choice
// see : http://www-apr.lip6.fr/~genitrini/publi/fsttcs13_genitrini.pdf

set min 100
set max 200

A ::= Ashuffle + Aplus
Ashuffle ::= SEQ(A) * <z>
Aplus ::= Ashuffle * Ashuffle * SEQ(Ashuffle)
