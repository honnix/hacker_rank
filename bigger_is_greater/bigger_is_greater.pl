next_permutation(S, Next) :-
    longest_non_increasing_suffix(S, Index1),
    PivotIndex is Index1 - 1,
    nth0(PivotIndex, S, Pivot),
    right_most_successor_to_pivot(S, Pivot, Index2),
    swap(S, PivotIndex, Index2, S1),
    split_at(S1, Index1, S2, S3),
    reverse(S3, S4),
    append(S2, S4, Next).

longest_non_increasing_suffix(S, Index) :-
    length(S, Length),
    Length1 is Length - 1,
    longest_non_increasing_suffix0(S, Length1, Index).

longest_non_increasing_suffix0(S, Pos, Pos) :-
    nth0(Pos, S, E1),
    nextto(E2, E1, S),
    E2 < E1, !.
longest_non_increasing_suffix0(S, Pos, Index) :-
    Pos1 is Pos - 1,
    longest_non_increasing_suffix0(S, Pos1, Index).

right_most_successor_to_pivot(S, Pivot, Index) :-
    length(S, Length),
    Length1 is Length - 1,
    right_most_successor_to_pivot0(S, Pivot, Length1, Index).

right_most_successor_to_pivot0(S, Pivot, Pos, Pos) :-
    nth0(Pos, S, E),
    E > Pivot, !.
right_most_successor_to_pivot0(S, Pivot, Pos, Index) :-
    Pos1 is Pos - 1,
    right_most_successor_to_pivot0(S, Pivot, Pos1, Index).

swap(S, Pos, Pos, S) :- !.
swap(S, Pos1, Pos2, Swapped) :-
    Pos1 < Pos2, !,
    swap(S, Pos2, Pos1, Swapped).
swap(S, Pos1, Pos2, Swapped) :-
    nth0(Pos1, S, E1, R1),
    nth0(Pos2, R1, E2, R2),
    nth0(Pos2, S1, E1, R2),
    nth0(Pos1, Swapped, E2, S1).

split_at(S, Pos, Pre, Sub) :-
    append(Pre, Sub, S),
    length(Pre, Pos), !.

bigger_is_greater(S, Bigger) :-
    atom_codes(S, C),
    nth0(0, C, E),
    \+ forall(member(X, C), E = X),
    next_permutation(C, C1),
    atom_codes(Bigger, C1).