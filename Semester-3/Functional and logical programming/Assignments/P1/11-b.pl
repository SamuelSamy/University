% b. Write a predicate to create the sublist (lm, ..., ln) from the list (l1,..., lk).


sublistMain([], _, _, _, []).
sublistMain(_, I, _, N, []) :-
    I > N, !.

sublistMain([_ | T], I, M, N, R) :-
    I < M,
    I1 is I + 1,
    sublistMain(T, I1, M, N, R).


sublistMain([H | T], I, M, N, [H | R]) :-
    I >= M, I =< N,
    I1 is I + 1,
    sublistMain(T, I1, M, N, R).


sublist(L, M, N, R) :-
    sublistMain(L, 1, M, N, R).