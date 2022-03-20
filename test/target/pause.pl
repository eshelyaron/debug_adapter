long_runnning(0) :- !.
long_runnning(N) :-
    sleep(1),
    M is N - 1,
    long_runnning(M).
