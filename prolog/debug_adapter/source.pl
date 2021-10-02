:- module(
       da_source,
       [
           da_source_subterm_span/5,
           da_source_layout_span/4
       ]
   ).


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
entry_port(redo(0)) :- !.

exit_port(fail) :- !.
exit_port(exit) :- !.
exit_port(exception(_)) :- !.


%!  da_source_layout_functor_span(+File, +Layout, -SourceSpan) is det.

:- det(da_source_layout_functor_span/3).
da_source_layout_functor_span(File, term_position(_, _, SO, EO, _), span(File, SL, SC, EL, EC)) :-
    da_source_file_offsets_line_column_pairs(File, [SO, EO], [SL-SC, EL-EC]).


%!  da_source_file_offsets_line_column_pairs(+File, +Offsets, -LineColumnPairs) is det.

da_source_file_offsets_line_column_pairs(File, Offsets, Pairs) :-
    setup_call_cleanup(
        ( prolog_clause:try_open_source(File, Stream),
          set_stream(Stream, newline(detect))
        ),
        da_source_stream_offsets_line_column_pairs(Stream, Offsets, Pairs),
        close(Stream)).


%!  da_source_stream_offsets_line_column_pairs(+Stream, +Offsets, -LineColumnPairs) is det.

da_source_stream_offsets_line_column_pairs(_Stream, [], []) :- !.
da_source_stream_offsets_line_column_pairs(Stream, [H0|T0], [Line-Column|T]) :-
    da_source_stream_offset_line_column(Stream, H0, Line, Column),
    da_source_stream_offsets_line_column_pairs(Stream, T0, T).


%!  da_source_stream_offset_line_column(+File, +Offset, -Line, -Column) is det.

da_source_stream_offset_line_column(Stream, Offset, Line, Column) :-
    character_count(Stream, CurrentOffset),
    (   CurrentOffset == Offset
    ->  line_count(Stream, Line),
        line_position(Stream, Column)
    ;   get_code(Stream, _),
        da_source_stream_offset_line_column(Stream, Offset, Line, Column)
    ).
