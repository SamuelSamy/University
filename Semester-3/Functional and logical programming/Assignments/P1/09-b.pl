% b. Define a predicate to determine the greatest common divisor of all numbers from a list.

gcd(D, D, D).
gcd(A, B, D) :-
    A > B, !,
    A1 is A - B,
    gcd(A1, B, D).

gcd(A, B, D) :-
    B1 is B - A,
    gcd(A, B1, D).


gcdList([V], V).
gcdList([H | T], D) :-
    gcdList(T, D1),
    gcd(H, D1, D).

