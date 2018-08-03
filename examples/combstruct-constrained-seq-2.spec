set min 1000
set max 10000
set try 5000

T = Union(Prod(Z, Sequence(T, 4 <= card)), Prod(Z, Sequence(T, 2 >= card)), Prod(Z, Sequence(T, card = 3)))

