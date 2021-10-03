:- module(
       da_clause,
       [
           da_clause_source_span/2,
           da_clause_source_span/3,
           da_clause_variable_names/2,
           qualified/3
       ]
   ).

:- use_module(source).

/** <module> DAP library module for reasoning about Prolog clauses

This module contains predicates for retrieving information about Prolog _clauses_ for debugging
purposes, such as relating clauses to their source code.

*/

%!  da_clause_variable_names(+ClauseRef, -VarNames) is det.

:- det(da_clause_variable_names/2).
da_clause_variable_names(null, varnames) :- !.
da_clause_variable_names(ClauseRef, VarNames) :-
    da_clause_decompiled(ClauseRef, Module, DecompiledClause, VariablesOffset),
    da_clause_source_term(ClauseRef, Module, DecompiledClause, VariablesOffset, _, _, _, VarNames).

%!  da_clause_source_span(+ClauseRef, -SourceSpan) is det.

:- det(da_clause_source_span/2).
da_clause_source_span(ClauseRef, SourceSpan) :-
    da_clause_source_span(ClauseRef, SourceSpan, []).


%!  da_clause_source_span(+ClauseRef, -SourceSpan, +Options) is det.

:- det(da_clause_source_span/3).
da_clause_source_span(null, reference(0), _Options) :- !.
da_clause_source_span(ClauseRef, SourceSpan, Options) :-
    option(pc(PC), Options),
    !,
    '$clause_term_position'(ClauseRef, PC, SubTermLayoutPath),
    da_clause_subterm_source_span(ClauseRef, SubTermLayoutPath, SourceSpan, Options).
da_clause_source_span(ClauseRef, SourceSpan, Options) :-
    da_clause_source_layout(ClauseRef, SourceFile, SourceLayout),
    (   SourceFile == null
    ->  da_clause_subterm_source_reference(ClauseRef, [], SourceReference, Options),
        SourceSpan = reference(SourceReference)
    ;   da_source_layout_span(SourceFile, SourceLayout, SourceSpan, Options)
    ).


%%!  da_clause_pc_call_site_source_span(+ClauseRef, +PC, -SourceSpan) is det.
%
%:- det(da_clause_pc_call_site_source_span/3).
%da_clause_pc_call_site_source_span(ClauseRef, PC, SourceSpan) :-
%    '$clause_term_position'(ClauseRef, PC, SubTermLayoutPath),
%    da_clause_port_subterm_source_span(ClauseRef, null, SubTermLayoutPath, SourceSpan).


%!  da_clause_subterm_source_span(+ClauseRef, +SubTermLayoutPath, -SourceSpan, +Options) is det.

:- det(da_clause_subterm_source_span/4).
da_clause_subterm_source_span(ClauseRef, SubTermLayoutPath, SourceSpan, Options) :-
    da_clause_source_layout(ClauseRef, SourceFile, SourceLayout),
    (   SourceFile == null
    ->  da_clause_subterm_source_reference(ClauseRef, SubTermLayoutPath, SourceReference, Options),
        SourceSpan = reference(SourceReference)
    ;   da_source_subterm_span(SourceFile, SourceLayout, SubTermLayoutPath, SourceSpan, Options)
    ).

%!  da_clause_source_layout(+ClauseRef, -SourceFile, -SourceLayout) is det.

:- det(da_clause_source_layout/3).
da_clause_source_layout(ClauseRef, SourceFile, SourceLayout) :-
    da_clause_decompiled(ClauseRef, Module, DecompiledClause, VariablesOffset),
    da_clause_source_term(ClauseRef, Module, DecompiledClause, VariablesOffset, _SourceClause, SourceFile, SourceLayout).

%!  da_clause_source_term(+ClauseRef, +Module, +DecompiledClause, +VariablesOffset, -SourceClause, -SourceFile, -SourceLayout) is det.

:- det(da_clause_source_term/7).
da_clause_source_term(ClauseRef, Module, DecompiledClause, VariablesOffset, SourceClause, SourceFile, SourceLayout) :-
    da_clause_source_term(ClauseRef, Module, DecompiledClause, VariablesOffset, SourceClause, SourceFile, SourceLayout, _VarNames).


%!  da_clause_source_term(+ClauseRef, +Module, +DecompiledClause, +VariablesOffset, -SourceClause, -SourceFile, -SourceLayout, VarNames) is det.

:- det(da_clause_source_term/8).
da_clause_source_term(ClauseRef, Module, DecompiledClause, VariablesOffset, SourceClause, SourceFile, SourceLayout, VarNames) :-
    clause_property(ClauseRef, file(SourceFile)),
    SourceFile \== user,
    !,
    clause_property(ClauseRef, line_count(Line)),
    setup_call_cleanup(prolog_clause:try_open_source(SourceFile, Stream),
                       read_source_term_at_location(Stream,
                                                    SourceClause,
                                                    [ line(Line),
                                                      module(Module),
                                                      subterm_positions(SourceLayout0),
                                                      variable_names(VarNames0)
                                                    ]),
                       close(Stream)),
    prolog_clause:unify_clause(SourceClause, DecompiledClause, Module, SourceLayout0, SourceLayout),
    prolog_clause:make_varnames(SourceClause, DecompiledClause, VariablesOffset, VarNames0, VarNames).
da_clause_source_term(_ClauseRef, Module, DecompiledClause, VariablesOffset, SourceClause, null, SourceLayout, VarNames) :-
    setup_call_cleanup(new_memory_file(MemFile),
                       ( setup_call_cleanup(open_memory_file(MemFile, write, MemOut),
                                            portray_clause(MemOut, DecompiledClause, [module(Module)]),
                                            close(MemOut)),
                         setup_call_cleanup(open_memory_file(MemFile, read, MemIn),
                                            read_source_term_at_location(MemIn, SourceClause,
                                                                         [ module(Module),
                                                                           subterm_positions(SourceLayout0),
                                                                           variable_names(VarNames0)
                                                                         ]),
                                           close(MemIn))),
                       free_memory_file(MemFile)),
    prolog_clause:unify_clause(SourceClause, DecompiledClause, Module, SourceLayout0, SourceLayout),
    prolog_clause:make_varnames(SourceClause, DecompiledClause, VariablesOffset, VarNames0, VarNames).

%!  da_clause_decompiled(+ClauseRef, +Module, +DecompiledClause) is det.

:- det(da_clause_decompiled/4).
da_clause_decompiled(ClauseRef, Module, DecompiledClause, VariablesOffset) :-
    '$clause'(Head0, Body, ClauseRef, VariablesOffset),
    qualified(Head0, Module, Head),
    (   Body == true
    ->  DecompiledClause = Head
    ;   DecompiledClause = (Head :- Body)
    ).


%!  da_clause_subterm_source_reference(+ClauseRef, +SubTermLayoutPath, +SourceReference, +Options) is det.

:- det(da_clause_subterm_source_reference/4).
da_clause_subterm_source_reference(ClauseRef, SubTermLayoutPath, SourceReference, _Options) :-
    (   da_clause_subterm_source_cached_reference(ClauseRef, SubTermLayoutPath, LastReference)
    ->  succ(LastReference, SourceReference)
    ;   SourceReference = 1
    ),
    asserta(da_clause_subterm_source_cached_reference(ClauseRef, SubTermLayoutPath, SourceReference)).


%!  da_clause_subterm_source_cached_reference(+ClauseRef, +SubTermLayoutPath, +SourceReference) is semidet.

:- thread_local da_clause_subterm_source_cached_reference/3.


qualified(Module:UnqualifiedGoal, Module, UnqualifiedGoal) :-
    !.
qualified('<meta-call>'(_Module0:Goal), Module, UnqualifiedGoal) :-
    qualified(Goal, Module, UnqualifiedGoal),
    !.
qualified('<meta-call>'(Goal), Module, UnqualifiedGoal) :-
    qualified(Goal, Module, UnqualifiedGoal),
    !.

qualified(UnqualifiedGoal, user, UnqualifiedGoal).
