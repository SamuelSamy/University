% b. For a heterogeneous list, formed from integer numbers and list of numbers, define a predicate to determine
% the maximum number of the list, and then to replace this value in sublists with the maximum value of sublist.

% [1, [2, 5, 7], 4, 5, [1, 4], 3, [1, 3, 5, 8, 5, 4], 5, [5, 9, 1], 2] =>
% [1, [2, 7, 7], 4, 5, [1, 4], 3, [1, 3, 8, 8, 8, 4], 5, [9, 9, 1], 2]


replace([], _, _, []) :- !.
replace([H | T], H, E, [E | R]) :-
    !, replace(T, H, E, R).
replace([H | T], V, E, [H | R]) :-
    replace(T, V, E, R).

max(A, B, A) :- A > B, !.
max(_, B, B).

maxList([H], H).
maxList([H | T], R) :-
    maxList(T, R1),
    max(H, R1, R).

maxHeterList([], 0).
maxHeterList([H], H) :- number(H), !.
maxHeterList([H | T], R) :-
    number(H), !,
    maxHeterList(T, R1),
    max(R1, H, R).

maxHeterList([_ | T], R) :-
    maxHeterList(T, R).


replaceMax([], _, []) :- !.
replaceMax([H | T], M, [RR | R]) :-
    is_list(H), !,
    maxList(H, SM),
    replace(H, M, SM, RR),
    replaceMax(T, M, R).

replaceMax([H | T], M, [H | R]) :-
    replaceMax(T, M, R).

main(L, R) :-
    maxHeterList(L, M),
    print(M),
    replaceMax(L, M, R).
