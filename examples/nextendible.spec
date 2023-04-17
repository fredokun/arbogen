// N-extendible Posets represented by their modular decomposition tree

N         ::= Nseries + Nparallel + Nspider + Nleaf
Nseries   ::= <z> * (Nseries + Nspider + Nleaf) * (Nparallel + Nspider + Nleaf)
Nparallel ::= <z> * (Nseries + Nspider + Nleaf) * (Nseries + Nspider + Nleaf)
Nspider   ::= <z> * <z> * <z> * (Nseries + Nparallel + Nspider + Nleaf) * Nsingle * Nsingle * Nsingle * Ntwo
Nleaf ::= Nsingle +  NM + NW + NOC5

Nsingle ::= <z>
NM ::= <z>
NW ::= <z>
NOC5 ::= <z>

Ntwo ::= Nsingle + Nanti + Nchain
Nanti ::= <z>
Nchain ::= <z>


