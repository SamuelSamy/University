% b. Write a predicate to determine the set of all the pairs of elements in a list. 
% L = [a b c d] => [[a b] [a c] [a d] [b c] [b d] [c d]].

computeSet([], _, []).
computeSet([H | T], A, [[A, H] | R]) :-
    computeSet(T, A, R).

unite([], L, L).
unite([H | T], L, [H | R]) :-
    unite(T, L, R).

main([_], []).
main([H | T], R) :-
    computeSet(T, H, R1),
    main(T, R2),
    unite(R1, R2, R).