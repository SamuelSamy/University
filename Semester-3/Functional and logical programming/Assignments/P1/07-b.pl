% b. Write a predicate to create a list (m, ..., n) of all integer numbers from the interval[m, n].

createList(N, N, [N]).
createList(M, N, [M | R]) :-
    M < N,
    M1 is M + 1,
    createList(M1, N, R).