:- module(
       da_breakpoint,
       [
           da_breakpoints_set/3
       ]
   ).


:- use_module(source).
:- use_module(frame).
:- use_module(library(prolog_breakpoints)).


:- multifile prolog:message//1.

prolog:message(log_message(BP, Map, String0)) -->
    { interpolate_string(String0, String, Map, []) },
    !,
    [ 'Log point ~w: ~w'-[BP, String] ].


:- dynamic da_known_breakpoint/7.
:- dynamic da_known_function_breakpoint/1.


prolog:break_hook(Clause, PC, FR, BFR, Expression, Action) :-
    da_known_breakpoint(BP, Clause, PC, Cond, Hit0, Hit, Log),
    da_break_hook(BP, Clause, PC, FR, BFR, Expression, Cond, Hit0, Hit, Log, Action).


:- det(da_break_hook/11).
da_break_hook(BP, Clause, PC, FR, _BFR, _Expression, Cond, Hit, Hit, Log, Action) :-
    !,
    retractall(da_known_breakpoint(BP, _, _, _, _, _, _)),
    asserta(da_known_breakpoint(BP, Clause, PC, Cond, 0, Hit, Log)),
    da_frame_evaluate(FR, Cond, Result, _),
    da_breakpoint_action(BP, FR, Result, Log, Action).
da_break_hook(BP, Clause, PC, _FR, _BFR, _Expression, Cond, Hit0, Hit, Log, continue) :-
    retractall(da_known_breakpoint(BP, _, _, _, _, _, _)),
    Hit1 is Hit0 + 1,
    asserta(da_known_breakpoint(BP, Clause, PC, Cond, Hit1, Hit, Log)).


:- det(da_breakpoint_action/5).
da_breakpoint_action( BP, _FR, true, null               , trace   ) :- !,
    retractall(da_tracer:da_tracer_last_action(_)),
    asserta(da_tracer:da_tracer_last_action(breakpoint(BP))).
da_breakpoint_action(_BP, _FR, _   , null               , continue) :- !.
da_breakpoint_action( BP,  FR, true, log_message(String), continue) :- !,
    da_frame_variables_mapping(FR, Map),
    print_message(trace, log_message(BP, Map, String)).
da_breakpoint_action(_BP, _FR, _   , _                  , continue).


:- det(da_breakpoints_set/3).
da_breakpoints_set(path(Path), Req, Res) :-
    user:ensure_loaded(Path),
    forall(da_breakpoint_path(BP, Path),
           da_breakpoint_delete(BP)),
    phrase(da_breakpoints_set(Req, path(Path)), Res).


da_breakpoint_delete(BP) :-
    catch(ignore(prolog_breakpoints:delete_breakpoint(BP)), _, true),
    retractall(da_known_breakpoint(BP, _, _, _, _, _, _)).


da_breakpoint_path(BP, Path) :-
    prolog_breakpoints:breakpoint_property(BP, file(Path)).


da_breakpoints_set([   ], _) --> [].
da_breakpoints_set([H|T], P) --> da_breakpoint_set(H, P), da_breakpoints_set(T, P).


:- det(da_breakpoint_set/4).
da_breakpoint_set(source_breakpoint(L0, C0, Cond, Hit, Log), path(P)) -->
    {   prolog_breakpoints:set_breakpoint(P, L0, C0, BP)   },
    !,
    {   prolog_breakpoints:known_breakpoint(Clause, PC, _, BP),
        asserta(da_known_breakpoint(BP, Clause, PC, Cond, 0, Hit, Log)),
        prolog_breakpoints:breakpoint_property(BP, character_range(A, L)),
        Z is A + L,
        da_source_file_offsets_line_column_pairs(path(P), [A, Z], [SL-SC, EL-EC])
    },
    [   breakpoint(BP, true, null, span(path(P), SL, SC, EL, EC))   ].
da_breakpoint_set(_, _) --> [].
