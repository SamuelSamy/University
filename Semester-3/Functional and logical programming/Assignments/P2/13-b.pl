% b. For a heterogeneous list, formed from integer numbers and list of numbers; write a predicate to delete from
% every sublist all sequences of consecutive values.

% [1, [2, 3, 5], 9, [1, 2, 4, 3, 4, 5, 7, 9], 11, [5, 8, 2], 7] =>
% [1, [5], 9, [4, 7, 9], 11, [5, 8, 2], 7] 

remove([], []) :- !.
remove([H], [H]) :- !.

remove([H1, H2], []) :- 
    H2 =:= H1 + 1, !.


remove([H1, H2, H3 | T], R) :-
    H2 =:= H1 + 1,
    H3 =:= H2 + 1, !,
    remove([H2, H3 | T], R).

remove([H1, H2, H3 | T], R) :-
    H2 =:= H1 + 1,
    H3 =\= H2 + 1, !,
    remove([H3| T], R).

remove([H | T], [H | R]) :-
    remove(T, R).


main([], []) :- !.
main([H | T], [RR | R]) :-
    is_list(H), !,
    remove(H, RR),
    main(T, R).

main([H | T], [H | R]) :-
    main(T, R).
    