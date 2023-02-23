% A player wants to choose the predictionsfor 4 games.The predictions can be 1, X, 2. 
% Write a predicate to  generate  all  possible  variants considering that:
% - last prediction can’t be 2
% - no more  than two possible predictions X

% ^.*('X'.*){3,}.*

prediction(1).
prediction('X').
prediction(2).

% generate(N: number, I: number, L: list, R: list)
% flow: (i, i, i, o).
%                 
% Model:                          
%   generate(n, i, l)       =   - l, if n = i
%                               - predicate(x) U generate(n, i + 1, l), otherwise
generate(N, N, L, L) :- !.

generate(N, I, L, R) :-
    prediction(X),
    I1 is I + 1,
    generate(N, I1, [X | L], R).


%verify(L: list, C: number, LT: boolean)
%
% Model:
%   verify(l1 l2 ... ln, counter, last_two)      =      -   true, if n == 0 and counter <= 2 and last_two == 0
%                                                       -   verify(l2 l3 ... ln, counter + 1, last_two) if l1 == 'X'
%                                                       -   verify(l2 l3 ... ln, counter, last_two), if l1 == 2 or l1 == 'X'
%                                                       -   verify(l2 l3 ... ln, counter, 1), if n == 1 and ln == 2
%                                                       -   false, otherwise (counter > 2 or last_two = 1)
verify([], C, 0) :-
    C =< 2, !.

verify([H], _, _) :-
    H == 2, !,
    false.

verify([H | T], C, LT) :-
    H == 'X',
    C1 is C + 1, !,
    verify(T, C1, LT).

verify([_ | T], C, LT) :-
    verify(T, C, LT).


solution(R) :-
    generate(4, 0, [], R),
    verify(R, 0, 0).


main(R) :-
    findall(R1, solution(R1), R).
