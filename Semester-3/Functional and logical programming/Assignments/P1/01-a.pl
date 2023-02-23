% a. Write a predicate to determine the lowest common multiple of a list formed from integer numbers.

gcd(D, D, D).
gcd(A, B, D) :-
    A > B, !,
    A1 is A - B,
    gcd(A1, B, D).

gcd(A, B, D) :-
    B1 is B - A,
    gcd(A, B1, D).


lcm(A, B, M) :-
    gcd(A, B, D),
    M is A * B / D.

lcmList([H], H).
lcmList([H | T], M) :-
    lcmList(T, M1),
    gcd(M1, H, D),
    M is M1 * H / D.

