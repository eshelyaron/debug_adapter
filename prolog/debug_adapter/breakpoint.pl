:- module(
       da_breakpoint,
       [
           da_breakpoints_set/3
       ]
   ).

:- use_module(source).
:- use_module(library(prolog_breakpoints)).

:- det(da_breakpoints_set/3).
da_breakpoints_set(Path, Req, Res) :-
    user:ensure_loaded(Path),
    findall(BP, prolog_breakpoints:breakpoint_property(BP, file(Path)), BPs),
    maplist(prolog_breakpoints:delete_breakpoint, BPs),
    phrase(da_breakpoints_set(Req, Path), Res).

da_breakpoints_set([   ], _) --> [].
da_breakpoints_set([H|T], P) --> da_breakpoint_set(H, P), da_breakpoints_set(T, P).

:- det(da_breakpoint_set/4).
da_breakpoint_set(source_breakpoint(L0, C0), P) -->
    {   prolog_breakpoints:set_breakpoint(P, L0, C0, BP)   },
    !,
    {   prolog_breakpoints:breakpoint_property(BP, character_range(A, L)),
        Z is A + L,
        da_source_file_offsets_line_column_pairs(P, [A, Z], [SL-SC, EL-EC])
    },
    [   breakpoint(BP, true, null, span(P, SL, SC, EL, EC))   ].
da_breakpoint_set(_, _) --> [].
