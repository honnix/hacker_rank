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

anagrams0([], [], L, L) :- !.
anagrams0([H1|T1], [H2|T2], L1, L2) :-
    char_code(H1, C1),
    char_code(H2, C2),
    Index1 is C1 - 95,
    Index2 is C2 - 95.

increase_at_index([H|T], CurrentIndex, CurrentIndex, L0, L) :-
    L1 = [H|L0]
