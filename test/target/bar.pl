:- module(
       bar,
       [
           bar/1,
           bar/2
       ]
   ).

:- dynamic bar/1.

bar(baz).

bar(Bar, Baz) :-
    (   bar(Bar), bar
    ;   bar(Baz)
    ).

bar :- phrase(bar(foo), `spam`).

bar(foo) --> `spam`.
