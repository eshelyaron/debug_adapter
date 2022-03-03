:- module(
       da_client,
       [
           dap_script/1
       ]
   ).

:- use_module(compat).
:- use_module(protocol).

:- thread_local dap_message/1.
:- thread_local dap_seq/1.

spawn_server(O, I, P) :-
    process_create(path(swipl),
                   ['-g', '[library(debug_adapter/main)]', '-t', 'halt'],
                   [ stdin(pipe(O)),
                     stdout(pipe(I)),
                     process(P)
                   ]),
    set_stream(I, buffer(full)),
    set_stream(I, newline(dos)),
    set_stream(O, buffer(false)).

halt_server(O, I, P) :-
    close(O),
    close(I),
    process_wait(P, Status, [timeout(0)]),
    (   Status == timeout
    ->  process_kill(P)
    ;   true
    ).

dap_script(S) :-
    setup_call_cleanup(
        (   spawn_server(O, I, P),
            asserta(dap_seq(1))
        ),

        maplist(dap_do(I, O), S),

        (   retractall(dap_message(_)),
            retractall(dap_seq(_)),
            halt_server(O, I, P)
        )
    ).

dap_do(I, O, event(Type)  ) :-
    dap_do(I, O, event(Type, _)), !.
dap_do(_, _, event(Type, Body)) :-
    dap_message(event(_, Type, Body)), !.
dap_do(_, _, call(Goal)   ) :-
    !,
    call(Goal).
dap_do(_, _, assertion(A) ) :-
    !,
    assertion(A).
dap_do(I, O, request(Type)) :-
    dap_do(I, O, request(Type, null)), !.
dap_do(_, O, request(Type, Body)) :-
    !,
    dap_seq(Seq0),
    !,
    dap_request(O, Seq0, Type, Body),
    Seq is Seq0 + 1,
    asserta(dap_seq(Seq)).
dap_do(I, O, reqres(Type)) :-
    dap_do(I, O, reqres(Type, null)), !.
dap_do(I, O, reqres(Type, Body)) :-
    dap_do(I, O, reqres(Type, Body, _)), !.
dap_do(I, O, reqres(Type, Body, Response)) :-
    !,
    dap_seq(ReqSeq),
    !,
    dap_do(I, O, request(Type, Body)),
    dap_do(I, O, response(ReqSeq, Type, Response)).
dap_do(_, _, response(ReqSeq, Type, Response)) :-
    dap_message(response(_, ReqSeq, Type, true, _, Response)), !.
dap_do(I, O, D            ) :-
    dap_next(I, N),
    asserta(dap_message(N)),
    dap_do(I, O, D).

dap_next(I, N) :-
    dap_read(I, R),
    _{ type : Type } :< R,
    (   Type == "response"
    ->  (   _{ seq: Seq, request_seq : ReqSeq, command : Command, success : Success } :< R,
            (   get_dict(message, R, Message)
            ->  true
            ;   Message = null
            ),
            (   get_dict(body, R, Body)
            ->  true
            ;   Body = null
            ),
            N = response(Seq, ReqSeq, Command, Success, Message, Body)
        )
    ;   Type == "event"
    ->  (   _{ seq : Seq, event : Event } :< R,
            (   get_dict(body, R, Body)
            ->  true
            ;   Body = null
            ),
            N = event(Seq, Event, Body)
        )
    ).
