:- dynamic
    solution/1.

generate([], S2, Acc, S) :- !,
    reverse(Acc, Acc1),
    append(Acc1, S2, S),
    new_solution(S).
generate(S1, [], Acc, S) :- !,
    reverse(Acc, Acc1),
    append(Acc1, S1, S),
    new_solution(S).
generate([H1|T1], [H2|T2], Acc, S) :-
    H1 < H2, !,
    Acc1 = [H1|Acc],
    no_worse(Acc1),
    generate(T1, [H2|T2], Acc1, S).
generate([H1|T1], [H2|T2], Acc, S) :-
    H1 > H2, !,
    Acc1 = [H2|Acc],
    no_worse(Acc1),
    generate([H1|T1], T2, Acc1, S).
generate([H1|T1], [H2|T2], Acc, S) :-
    Acc1 = [H1|Acc],
    greedy_read(H1, T1, T1S, Acc1, Acc2),
    no_worse(Acc2),
    generate(T1S, [H2|T2], Acc2, S);
    Acc1 = [H2|Acc],
    greedy_read(H2, T2, T2S, Acc1, Acc2),
    no_worse(Acc2),
    generate([H1|T1], T2S, Acc2, S).

new_solution(S) :-
    retractall(solution(_)),
    asserta(solution(S)).

greedy_read(E, [E|T], S, Acc1, Acc2) :- !,
    greedy_read(E, T, S, [E|Acc1], Acc2).
greedy_read(_, L, L, Acc, Acc).

no_worse(S) :-
    reverse(S, S1),
    (   solution(S0)
    ->  \+ compare(<, S0, S1)
    ;   true
    ).

morgan_and_a_string(S1, S2, Ss) :-
    atom_codes(S1, C1),
    atom_codes(S2, C2),
    findall(_, generate(C1, C2, [], _), _),
    retract(solution(Ss0)),
    atom_codes(Ss, Ss0).