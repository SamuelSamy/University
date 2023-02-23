% a. Write a predicate to test equality of two sets without using the set difference.

countOccurrences([], _, 0).
countOccurrences([H | T], H, R) :-
    !, countOccurrences(T, H, R1),
    R is R1 + 1.

countOccurrences([_ | T], E, R) :-
    countOccurrences(T, E, R).


testEqualityMain([], [], _, _).
testEqualityMain([HS1 | TS1], [HS2 | TS2], IS1, IS2) :-
    countOccurrences(IS1, HS1, C1),
    countOccurrences(IS2, HS1, C2),
    C1 =:= C2,

    countOccurrences(IS1, HS2, C3),
    countOccurrences(IS2, HS2, C4),
    C3 =:= C4,

    testEqualityMain(TS1, TS2, IS1, IS2).


testEquality(S1, S2) :-
    testEqualityMain(S1, S2, S1, S2).    

