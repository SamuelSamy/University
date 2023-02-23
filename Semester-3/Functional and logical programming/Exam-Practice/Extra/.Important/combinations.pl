comb([E | _], 1, [E]).

comb([_ | T], K, R) :-
    comb(T, K, R).

comb([H | T], K, [H | R]) :-
    K > 1,
    K1 is K - 1,
    comb(T, K1, R).

main(L, K, R) :-
    findall(R1, comb(L, K, R1), R).