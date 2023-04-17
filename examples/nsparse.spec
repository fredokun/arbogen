// N-sparse Posets represented by their modular decomposition tree

N         ::= Nseries + Nparallel + Nspider + Nsingle
Nseries   ::= <z> * (Nseries + Nspider + Nsingle) * (Nparallel + Nspider + Nsingle)
Nparallel ::= <z> * (Nseries + Nspider + Nsingle) * (Nseries + Nspider + Nsingle)
Nspider   ::= <z> * <z> * <z> * (Nseries + Nparallel + Nspider + Nsingle) * Nsingle * Nsingle * Nsingle * Nsingle
Nsingle ::= <z>
