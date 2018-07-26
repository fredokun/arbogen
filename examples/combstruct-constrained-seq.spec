set min 1000
set max 10000
set try 5000

T = Union(Prod(Z, A), Prod(Z, B), Prod(Z, Sequence(T, card = 3))),
A = Sequence(T, 4 <= card),
B = Sequence(T, 2 >= card)


