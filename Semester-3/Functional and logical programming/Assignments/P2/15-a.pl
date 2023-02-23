% a. Define a predicate to determine the predecessor of a number represented as digits in a list.
% [1 9 3 6 0 0] => [1 9 3 5 9 9]


predecessor([], 0, []) :- !.
predecessor([0], 1, [9]) :- !.
predecessor([X], 0, [X1]) :-
    X1 is X - 1, !.

predecessor([0 | T], 1, [9 | R]) :-
    predecessor(T, 1, R), !.

predecessor([H | T], 0, [H1 | R]) :-
    predecessor(T, C, R),
    H1 is H - C.



predecessor(L, R) :-
    predecessor(L, 0, R).