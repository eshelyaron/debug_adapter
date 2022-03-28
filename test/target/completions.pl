go :- 'Baz'(Bad), Bad = good.


'Bad' :-
    member(Foo, [Bar, Baz]),
    Bar = 'Bad',
    Baz = 'Baz',
    dif(Foo, Bar).

'Baz'(_) :- 'Bad'.
