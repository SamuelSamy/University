%b. For a heterogeneous list, formed from integer numbers and list of numbers, write a predicate to sort every
% sublist, keeping the doubles.

% [1, 2, [4, 1, 4], 3, 6, [7, 10, 1, 3, 9], 5, [1, 1, 1], 7] =>
% [1, 2, [1, 4, 4], 3, 6, [1, 3, 7, 9, 10], 5, [1, 1, 1], 7].

min(A, B, A) :- A < B, !.
min(_, B, B).

minList([E], E) :- !.
minList([H | T], R) :-
    minList(T, R1),
    min(H, R1, R).

removeFirstElement([], _, []).
removeFirstElement([H | T], H, T) :- !.
removeFirstElement([H | T], E, [H | R]) :-
    removeFirstElement(T, E, R).


mySort([], []).
mySort(L, [V | R]) :-
    minList(L, V),
    removeFirstElement(L, V, LR),
    mySort(LR, R).


main([], []) :- !.
main([H | T], [HS | R]) :-
    is_list(H), !,
    mySort(H, HS),
    main(T, R).

main([H | T], [H | R]) :-
    main(T, R).

