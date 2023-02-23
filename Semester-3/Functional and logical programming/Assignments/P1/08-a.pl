% a. Write a predicate to determine if a list has even numbers of elements without counting the elements from the list.

isEvenList([]).
isEvenList([_, _ | T]) :-
    isEvenList(T).