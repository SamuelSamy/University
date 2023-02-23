% b. Write a predicate to add value 1 after every even element from a list.

addOneAfterEven([], []).

addOneAfterEven([H | T], [H, 1 | R]) :-
    H mod 2 =:= 0, !,
    addOneAfterEven(T, R).

addOneAfterEven([H | T], [H | R]) :-
    addOneAfterEven(T, R).