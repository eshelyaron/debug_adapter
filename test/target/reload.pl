break_here :-
    true,
    true.


this_predicate :-
    source_file(this_predicate, ThisFile),
    load_files([ThisFile]),
    garbage_collect_clauses,
    true.
