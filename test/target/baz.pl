baz :-
    baz(1),
    baz(2),
    baz(3),
    baz(4).

baz(X) :-
    string(X).
baz(X) :-
    number(X).
