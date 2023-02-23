% a. Write a predicate to remove all occurrences of a certain atom from a list.

removeOccurrences([], _, []).

removeOccurrences([V | T], V, R) :-
    removeOccurrences(T, V, R).

removeOccurrences([H | T], V, [H | R]) :-
    removeOccurrences(T, V, R).