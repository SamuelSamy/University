insert(E, L, [E | L]).
insert(E, [H | T], [H | R]) :-
    insert(E, T, R).

perm([], []).
perm([H | T], R) :-
    perm(T, R1),
    insert(H, R1, R).

main(L, R) :-
    findall(R1, perm(L, R1), R).