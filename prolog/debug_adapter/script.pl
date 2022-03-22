:- module(da_script,
          [
              run_script/2   % run_script(+Path, +Options)
          ]
         ).

:- use_module(compat).
:- use_module(session).

:- predicate_options(run_script/2, 2, [ server_executable(+any),
                                        server_cli_args(+list(atom)),
                                        bindings(+list(any))]).

%! run_script(Path, Options) is det.
%
%  Main entry point for executing `dapscript`s. Reads the script from
%  the file located at Path.
%
%  Supported Options are:
%  * server_executable(+Exec)
%    Exec specifies the path of the DAP server executable to run the script against.
%    Defaults to the `swipl` executable.
%  * server_cli_args(+Args)
%    Args is a list of command line arguments that will be passed to the
%    DAP server executable. Defaults to instructing `swipl` to load and run the SWI-Prolog debug
%    adapter server from `library(debug_adapter/main)`.
%  * bindings(+Bindings)
%    Bindings is a list of variable bindings given as _Name_ = _Value_ pairs that will be made available
%    in the context of the executed `dapscript`. Defaults to `[]`.
run_script(Path, Options) :-
    swipl_executable(Swipl),
    option(server_executable(Exec), Options, Swipl),
    option(server_cli_args(Args), Options, ['-g', '[library(debug_adapter/main)]', '-t', 'halt', '--', '-T', '10']),
    option(bindings(Vars), Options, []),
    setup_call_cleanup(open(Path, read, In),
                       setup_call_cleanup(session_start([Exec|Args], Session),
                                          run_script_from_stream(In, Vars, Session),
                                          session_stop(Session)),
                       close(In)).

run_script_from_stream(In, Vars0, Session) :-
    read_term(In, Term, [variable_names(Vars1)]),
    unify_vars(Vars1, Vars0, Vars),
    execute_term(Term, In, Vars, Session).

:- det(unify_vars/3).
unify_vars([], Vars, Vars) :- !.
unify_vars([H|T], Vars0, Vars) :-
    memberchk(H, Vars0), !,
    unify_vars(T, Vars0, Vars).
unify_vars([H|T], Vars0, [H|Vars]) :-
    unify_vars(T, Vars0, Vars).

execute_term(end_of_file, _, _, _).
execute_term(?- Goal, In, Vars, Session) :-
    Goal,
    run_script_from_stream(In, Vars, Session).
execute_term(Kind :- Body, In, Vars, Session0) :-
    semicolon_list(Body, Parts),
    foldl(execute_part_(Kind), Parts, Session0, Session),
    run_script_from_stream(In, Vars, Session).

execute_part_(Kind, Part, Session0, Session) :-
    debug(dap(script), "Executing ~p :- ~p", [Kind, Part]),
    execute_part(Kind, Part, Session0, Session).
execute_part(request, (Type:Req -> Res), Session0, Session) :-
    debug(dap(test), "here", []),
    !, session_request_response(Type, Req, Res, Session0, Session).
execute_part(request, (Type -> Res), Session0, Session) :-
    debug(dap(test), "there", []),
    !, session_request_response(Type, null, Res, Session0, Session),
    debug(dap(test), "there ~w", [Res]).
execute_part(request, (Type:Req *-> Success:Res0), Session0, Session) :-
    !, session_request_response(Type, Req, Success:Res, Session0, Session), Res0 >:< Res.
execute_part(request, (Type *-> Success:Res0), Session0, Session) :-
    !, session_request_response(Type, null, Success:Res, Session0, Session), Res0 >:< Res.
execute_part(request, (Type:Req), Session0, Session) :-
    !, session_request_response(Type, Req, _, Session0, Session).
execute_part(request, Type, Session0, Session) :-
    !, session_request_response(Type, null, _, Session0, Session).
execute_part(event, Type:Body, Session0, Session) :-
    !, session_event(Type, Body, Session0, Session), !.
execute_part(event, Type:<Body0, Session0, Session) :-
    !, session_event(Type, Body, Session0, Session), Body0 >:< Body, !.
execute_part(event, Type, Session0, Session) :-
    !, session_event(Type, _, Session0, Session), !.
execute_part(reverse, Type:Body, Session0, Session) :-
    !, session_reverse_request(Type, Body, Session0, Session), !.
execute_part(reverse, Type, Session0, Session) :-
    !, session_reverse_request(Type, _, Session0, Session), !.


swipl_executable('C:\\Program Files\\swipl\\bin\\swipl.exe') :-
    current_prolog_flag(windows, true),
    !.
swipl_executable(path(swipl)).
