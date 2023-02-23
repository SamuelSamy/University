% a. Merge two sorted lists with removing the double values.

removeOccurrences([], _, []) :- !.
removeOccurrences([H | T], H, R) :- 
    !, removeOccurrences(T, H, R).
removeOccurrences([H | T], E, [H | R]) :-
    removeOccurrences(T, E, R).

merge([], L, L) :- !.
merge(L, [], L) :- !.

merge([H1 | T1], [H1 | T2], R) :-
    !, merge(T1, [H1 | T2], R).

merge([H1 | T1], [H2 | T2], [H1 | R]) :-
    H1 < H2, !,
    removeOccurrences(T1, H1, NT1),
    removeOccurrences(T2, H1, NT2),
    merge(NT1, [H2 | NT2], R).


merge([H1 | T1], [H2 | T2], [H2 | R]) :-
    removeOccurrences(T1, H2, NT1),
    removeOccurrences(T2, H2, NT2),
    merge([H1 | NT1], NT2, R).

