set eps1 0.0000000001
set eps2 0.0000000001

// Fork-Join-Plus graphs, counting global choices

E     ::= Epar + Eplus
Epar  ::= <z> + <z> * E + <z> * E * E * SEQ(E) * E
Eplus ::= Epar * Fpar * SEQ(Fpar) * E
          + Fpar * SEQ(Fpar) * Epar * SEQ(Fpar) * E

// Fork-Join-Plus graphs

F     ::= Fpar + Fplus
Fpar  ::= <z> + <z> * F + <z> * F * F * SEQ(F) * F
Fplus ::= Fpar * Fpar * SEQ(Fpar) * F
