% a. Write a predicate to test if a list is a set.

countOccurrences([], _, 0).
countOccurrences([V | T], V, C) :-
    countOccurrences(T, V, C1),
    C is C1 + 1.

countOccurrences([H | T], V, C) :-
    H =\= V,
    countOccurrences(T, V, C). 

isSet([]).
isSet([H | T]) :-
    countOccurrences(T, H, C),
    C =:= 0,
    isSet(T).