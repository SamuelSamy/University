% b. Calculate the alternate sum of list’s elements (l1 - l2 + l3 ...).

sum([], 0).
sum([H], H).

sum([H1, H2 | T], R) :-
    sum(T, R1),
    R is H1 - H2 + R1.

