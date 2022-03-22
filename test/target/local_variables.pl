arity_zero :-
    A = B,
    B = A,
    X = 1,
    Y = 2,
    Z is max(X, Y),
    Z > 0,
    true.
