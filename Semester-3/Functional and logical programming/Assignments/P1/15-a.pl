% a. Write a predicateto transform a list in a set, considering the first occurrence.
% [1,2,3,1,2] is transform in [1,2,3].

removeOccurrences([], _, []).
removeOccurrences([E | T], E, R) :-
    removeOccurrences(T, E, R), !.

removeOccurrences([H | T], E, [H | R]) :-
    removeOccurrences(T, E, R).

listToSet([], []).
listToSet([H | T], [H | R]) :-
    removeOccurrences(T, H, NT),
    listToSet(NT, R).