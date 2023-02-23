% b. Write a predicate to select the n-th element of a given list.

selectMain([H | _], N, N, H) :- !.
selectMain([_ | T], N, I, R) :-
    I1 is I + 1,
    selectMain(T, N, I1, R).

select(L, N, R) :-
    selectMain(L, N, 1, R).