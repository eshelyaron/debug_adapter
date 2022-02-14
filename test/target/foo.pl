:- module(
       foo,
       [
           foo/0,
           foo/1
       ]
   ).

:- use_module(bar).

foo :-
    asserta((bar(Foo) :- (Foo = spam -> true; Foo = foo))),
    bar(foo, baz),
    foo(bar).

foo(bar) :-
    foo(1, 2, 3, 4, 5),
    !,
    foo(baz, bar).

foo(baz) :-
    foo(baz, bar).

foo(Foo) :-
    foo(baz, Foo).

foo(Bar, Baz) :-
    (   Bar = bar
    ->  (   Baz = baz
        ;   false
        )
    ;   Bar = baz
    ->  (   Baz = bar
        ;   false
        )
    ).

foo(A, B, C, D, E) :-
    E is (D + C - B) * A.
