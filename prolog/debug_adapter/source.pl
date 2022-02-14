:- module(
       da_source,
       [
           da_source_subterm_span/5,
           da_source_layout_span/4,
           da_source_clause_cached_reference/2,
           da_source_clause_reference/3,
           da_clause_decompiled/4,
           qualified/3,
           da_source_file_offsets_line_column_pairs/3
       ]
   ).

:- use_module(library(prolog_clause)).

/** <module> DAP library module for reasoning about Prolog sources

This module contains predicates for retrieving information about the _spans_ and _layouts_ of Prolog
source terms for debugging purposes.

*/

%!  da_source_subterm_span(+SourceFile, +SourceLayout, +SubTermPositionPath, -SubTermSourceSpan, +Options) is det.

:- det(da_source_subterm_span/5).
da_source_subterm_span(File, Layout, SubTermPositionPath, SubTermSpan, Options) :-
    da_source_subterm_layout(Layout, SubTermPositionPath, SubTermLayout),
    da_source_layout_span(File, SubTermLayout, SubTermSpan, Options).


%!  da_source_subterm_layout(+Layout, +SubTermLayoutPath, -SubTermLayout) is det.

:- det(da_source_subterm_layout/3).
da_source_subterm_layout(Layout, _Path, Layout) :-
    var(Layout),
    !.
da_source_subterm_layout(term_position(_, _, _, _, PosL), [A|T], SPos) :-
    nth1(A, PosL, Pos),
    !,
    da_source_subterm_layout(Pos, T, SPos).
da_source_subterm_layout(brace_term_position(_,_,Pos), [1|T], SPos) :-
    !,
    da_source_subterm_layout(Pos, T, SPos).
da_source_subterm_layout(parentheses_term_position(_,_,Pos), Path, SPos) :-
    !,
    da_source_subterm_layout(Pos, Path, SPos).
da_source_subterm_layout(Layout, _Path, Layout).


%!  da_source_layout_span(+File, +Layout, -SourceSpan, +Options) is det.

:- det(da_source_layout_span/4).
da_source_layout_span(File, Layout, SourceSpan, Options) :-
    option(port(Port), Options, call),
    da_source_layout_port_span(File, Layout, Port, SourceSpan, Options).


%!  da_source_layout_port_span(+File, +Layout, +Port, -SourceSpan, +Options) is det.

:- det(da_source_layout_port_span/5).
da_source_layout_port_span(File, Layout, Port, span(File, SL, SC, EL, EC), _Options) :-
    entry_port(Port),
    !,
    arg(1, Layout, SO),
    arg(2, Layout, EO),
    da_source_file_offsets_line_column_pairs(File, [SO, EO], [SL-SC, EL-EC]).
da_source_layout_port_span(File, Layout, Port, span(File, SL, SC, EL, EC), _Options) :-
    exit_port(Port),
    !,
    arg(2, Layout, SO),
    succ(SO, EO),
    da_source_file_offsets_line_column_pairs(File, [SO, EO], [SL-SC, EL-EC]).
da_source_layout_port_span(File, Layout, _Port, SourceSpan, _Options) :-
    da_source_layout_functor_span(File, Layout, SourceSpan).


entry_port(call) :- !.
entry_port(cut_call(_)) :- !.
entry_port(redo(0)) :- !.

exit_port(fail) :- !.
exit_port(exit) :- !.
exit_port(cut_exit(_)) :- !.
exit_port(exception(_)) :- !.


%!  da_source_layout_functor_span(+File, +Layout, -SourceSpan) is det.

:- det(da_source_layout_functor_span/3).
da_source_layout_functor_span(File, SO-EO, span(File, SL, SC, EL, EC)) :-
    !,
    da_source_file_offsets_line_column_pairs(File, [SO, EO], [SL-SC, EL-EC]).
da_source_layout_functor_span(File, term_position(_, _, SO, EO, _), span(File, SL, SC, EL, EC)) :-
    da_source_file_offsets_line_column_pairs(File, [SO, EO], [SL-SC, EL-EC]).


%!  da_source_file_offsets_line_column_pairs(+File, +Offsets, -LineColumnPairs) is det.

:- det(da_source_file_offsets_line_column_pairs/3).
da_source_file_offsets_line_column_pairs(path(File), Offsets, Pairs) :-
    !,
    setup_call_cleanup(
        ( prolog_clause:try_open_source(File, Stream),
          set_stream(Stream, newline(detect))
        ),
        da_source_stream_offsets_line_column_pairs(Stream, Offsets, Pairs),
        close(Stream)).
da_source_file_offsets_line_column_pairs(reference(0), Offsets, Pairs) :- !, findall(0-0, member(_, Offsets), Pairs).
da_source_file_offsets_line_column_pairs(reference(SourceReference), Offsets, Pairs) :-
    da_source_clause_cached_reference(ClauseRef, SourceReference),  % TODO - cache newline positions for cached clauses
    da_clause_decompiled(ClauseRef, Module, DecompiledClause, _),
    setup_call_cleanup(new_memory_file(MemFile),
                       ( setup_call_cleanup(open_memory_file(MemFile, write, MemOut),
                                            portray_clause(MemOut, DecompiledClause, [module(Module)]),
                                            close(MemOut)),
                         setup_call_cleanup(open_memory_file(MemFile, read, MemIn),
                                            da_source_stream_offsets_line_column_pairs(MemIn, Offsets, Pairs),
                                            close(MemIn))),
                       free_memory_file(MemFile)).


%!  da_source_stream_offsets_line_column_pairs(+Stream, +Offsets, -LineColumnPairs) is det.

da_source_stream_offsets_line_column_pairs(_Stream, [], []) :- !.
da_source_stream_offsets_line_column_pairs(Stream, [H0|T0], [Line-Column|T]) :-
    da_source_stream_offset_line_column(Stream, H0, Line, Column),
    da_source_stream_offsets_line_column_pairs(Stream, T0, T).


%!  da_source_stream_offset_line_column(+File, +Offset, -Line, -Column) is det.
%!  da_source_stream_offset_line_column(+File, -Offset, +Line, +Column) is det.

da_source_stream_offset_line_column(Stream, Offset, Line, Column) :-
    var(Offset),
    !,
    line_count(Stream, CurrentLine),
    (   CurrentLine == Line
    ->  character_count(Stream, CurrentOffset),
        Offset is CurrentOffset + Column
    ;   skip_line(Stream),
        da_source_stream_offset_line_column(Stream, Offset, Line, Column)
    ).
da_source_stream_offset_line_column(Stream, Offset, Line, Column) :-
    character_count(Stream, CurrentOffset),
    (   CurrentOffset == Offset
    ->  line_count(Stream, Line),
        line_position(Stream, Column)
    ;   get_code(Stream, _),
        da_source_stream_offset_line_column(Stream, Offset, Line, Column)
    ).


%!  da_source_clause_reference(+ClauseRef, -SourceReference, +Options) is det.

:- det(da_source_clause_reference/3).
da_source_clause_reference(ClauseRef, SourceReference, _Options) :-
    (   da_source_clause_cached_reference(ClauseRef, LastReference)
    ->  succ(LastReference, SourceReference)
    ;   random_between(0, 16777216, SourceReference)
    ),
    asserta(da_source_clause_cached_reference(ClauseRef, SourceReference)).


%!  da_source_clause_cached_reference(+ClauseRef, +SourceReference) is semidet.

:- dynamic da_source_clause_cached_reference/2.


%!  da_clause_decompiled(+ClauseRef, -Module, -DecompiledClause, -VariablesOffset) is det.

:- det(da_clause_decompiled/4).
da_clause_decompiled(ClauseRef, Module, DecompiledClause, VariablesOffset) :-
    '$clause'(Head0, Body, ClauseRef, VariablesOffset),
    qualified(Head0, Module, Head),
    (   Body == true
    ->  DecompiledClause = Head
    ;   DecompiledClause = (Head :- Body)
    ).


qualified(Module:UnqualifiedGoal, Module, UnqualifiedGoal) :-
    !.
qualified('<meta-call>'(_Module0:Goal), Module, UnqualifiedGoal) :-
    qualified(Goal, Module, UnqualifiedGoal),
    !.
qualified('<meta-call>'(Goal), Module, UnqualifiedGoal) :-
    qualified(Goal, Module, UnqualifiedGoal),
    !.

qualified(UnqualifiedGoal, user, UnqualifiedGoal).
