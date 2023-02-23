% a. Define a predicate to remove from a list all repetitive elements. 
% [1,2,1,4,1,3,4] => l=[2,3]

removeOccurrences([], _, []).

removeOccurrences([V | T], V, R) :-
    removeOccurrences(T, V, R).

removeOccurrences([H | T], V, [H | R]) :-
    removeOccurrences(T, V, R).


countOccurrences([], _, 0).
countOccurrences([V | T], V, C) :-
    countOccurrences(T, V, C1),
    C is C1 + 1.

countOccurrences([H | T], V, C) :-
    H =\= V,
    countOccurrences(T, V, C). 


removeDuplicates([], []).
removeDuplicates([H | T], [H | R]) :-
    countOccurrences([H | T], H, C),
    C =:= 1,
    removeDuplicates(T, R).

removeDuplicates([H | T], R) :-
    countOccurrences([H | T], H, C),
    C =\= 1,
    removeOccurrences(T, H, NT),
    removeDuplicates(NT, R).