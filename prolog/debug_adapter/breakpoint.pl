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

prolog:message(log_message(BP, String)) -->
    [ 'Log point ~w: ~w'-[BP, String] ].


:- dynamic da_known_breakpoint/7.


prolog:break_hook(Clause, PC, FR, BFR, Expression, Action) :-
    da_known_breakpoint(BP, Clause, PC, Cond, Hit0, Hit, Log),
    da_break_hook(BP, Clause, PC, FR, BFR, Expression, Cond, Hit0, Hit, Log, Action).


:- det(da_break_hook/11).
da_break_hook(BP, Clause, PC, FR, _BFR, _Expression, Cond, Hit, Hit, Log, Action) :-
    !,
    retractall(da_known_breakpoint(BP, _, _, _, _, _, _)),
    asserta(da_known_breakpoint(BP, Clause, PC, Cond, 0, Hit, Log)),
    da_frame_evaluate(FR, Cond, Result, _),
    da_breakpoint_action(BP, Result, Log, Action).
da_break_hook(BP, Clause, PC, _FR, _BFR, _Expression, Cond, Hit0, Hit, Log, continue) :-
    retractall(da_known_breakpoint(BP, _, _, _, _, _, _)),
    Hit1 is Hit0 + 1,
    asserta(da_known_breakpoint(BP, Clause, PC, Cond, Hit1, Hit, Log)).


:- det(da_breakpoint_action/4).
da_breakpoint_action(_BP, true, null               , trace   ) :- !.
da_breakpoint_action(_BP, _   , null               , continue) :- !.
da_breakpoint_action( BP, true, log_message(String), continue) :- !, print_message(trace, log_message(BP, String)).
da_breakpoint_action(_BP, _   , _                  , continue).


:- det(da_breakpoints_set/3).
da_breakpoints_set(path(Path), Req, Res) :-
    user:ensure_loaded(Path),
    foreach(da_breakpoint_path(BP, Path),
            da_breakpoint_delete(BP)),
    phrase(da_breakpoints_set(Req, path(Path)), Res).


da_breakpoint_delete(BP) :-
    ignore(prolog_breakpoints:delete_breakpoint(BP)),
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
        debug(dap(breakpoint), "asserting ~w ~w ~w ~w 0 ~w ~w", [BP, Clause, PC, Cond, Hit, Log]),
        asserta(da_known_breakpoint(BP, Clause, PC, Cond, 0, Hit, Log)),
        prolog_breakpoints:breakpoint_property(BP, character_range(A, L)),
        Z is A + L,
        da_source_file_offsets_line_column_pairs(path(P), [A, Z], [SL-SC, EL-EC])
    },
    [   breakpoint(BP, true, null, span(path(P), SL, SC, EL, EC))   ].
da_breakpoint_set(_, _) --> [].
