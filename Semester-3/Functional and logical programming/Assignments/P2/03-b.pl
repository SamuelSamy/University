% b. For a heterogeneous list, formed from integer numbers and list of numbers, merge all sublists with removing
% the double values.

% [1, [2, 3], 4, 5, [1, 4, 6], 3, [1, 3, 7, 9, 10], 5, [1, 1, 11], 8] =>
% [1, 2, 3, 4, 6, 7, 9, 10, 11].

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


mergeSublists([], []) :- !.
mergeSublists([H | T], R) :-
    is_list(H), !,
    mergeSublists(T, R1),
    merge(H, R1, R).


mergeSublists([_ | T], R) :-
    mergeSublists(T, R).