% a. Given a linear numerical list write a predicate to remove all sequences of consecutive values.

% remove([1, 2, 4, 6, 7, 8, 10], L) will produce L=[4, 10].


remove([], []) :- !.
remove([H], [H]) :- !.

remove([H1, H2], []) :- 
    H2 =:= H1 + 1, !.


remove([H1, H2, H3 | T], R) :-
    H2 =:= H1 + 1,
    H3 =:= H2 + 1, !,
    remove([H2, H3 | T], R).

remove([H1, H2, H3 | T], R) :-
    H2 =:= H1 + 1,
    H3 =\= H2 + 1, !,
    remove([H3| T], R).

remove([H | T], [H | R]) :-
    remove(T, R).