:- use_module(library(clpfd)).

anagrams(I, J, X, Y, S, Max) :-
    [I, J, X, Y] ins 0..Max,
    J #>= I,
    X #>= I + 1,
    Offset #= J - I,
    X #=< Max - Offset,
    Y #= X + Offset,
    sublist(S, I, J, S1),
    sublist(S, X, Y, S2),
    msort(S1, S11),
    msort(S2, S22),
    S11 = S22.

sublist(L0, Index, Index, [E]) :-
    nth0(Index, L0, E).
sublist(L0, From, To, L) :-
    nth0(From, L0, E),
    L = [E|L1],
    From1 is From + 1,
    sublist(L0, From1, To, L1).