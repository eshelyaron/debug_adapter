is_a(rock1, rock).
is_a(rock2, rock).
color(rock1, red).

noun(X, Type) :- is_a(X, Type).
adjective(X, color, Value) :- color(X, Value).
test_noun1(X, Type) :-
    noun(X, Type).
test_noun2(X, Type) :-
    noun(X, Type).

bp :-
    test_noun2(rock1, rock),
    false.
bp :-
    test_noun1(rock1, rock).

