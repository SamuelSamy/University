% b.Define a predicate to produce a list of pairs (atom n) from an initial list of atoms. In this initial list atom has n occurrences.
% numberatom([1, 2, 1, 2, 1, 3, 1], X) => X = [[1, 4], [2, 2], [3, 1]].

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


numberAtom([], []).

numberAtom([H | T], [[H, C] | R]) :-
    countOccurrences([H | T], H, C),
    removeOccurrences(T, H, RR),
    numberAtom(RR, R).

