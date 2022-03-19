:- module(
       da_server,
       [
           da_server/1
       ]
   ).


/** <module> SWI-Prolog Debug Adapter Server

This module contains the core logic for handling DAP request sent by DAP clients which are most
commonly IDE extensions controlled interactivly by a progammer. The main entry point is da_server/1.

The implementation is most dominently guided by the [DAP
specification](https://microsoft.github.io/debug-adapter-protocol/specification).
*/


:- use_module(compat).
:- use_module(protocol).
:- use_module(sdk).

:- dynamic dap_server_thread_id/1.


:- predicate_options(da_server/1, 1, [ in(+stream),
                                       out(+stream),
                                       threads(+list(pair))
                                     ]
                    ).


%!  da_server(+Options) is det.
%
%   Starts the DAP server in the current thread. Options:
%    * in(+Stream)
%      Stream will be used by the server to read incoming messages from the client. Defaults to user_input.
%    * out(+Stream)
%      Stream will be used by the server to emit outgoing messages to the client. Defaults to user_output.
%    * threads(+Threads)
%      List of initial debuggee threads to be monitored by the server.
%    * setup(+Setup)
%    * on_command(+OnCommand)
%    * cleanup(+Cleanup)
%    * initial_state(+State)

:- det(da_server/1).
da_server(Options) :-
    option(in(In), Options, user_input),
    option(out(Out), Options, user_output),
    option(setup(Setup), Options, true),
    option(on_command(OnCommand), Options),
    option(cleanup(Cleanup), Options, true),
    option(initial_state(State), Options, []),
    set_stream(In, buffer(full)),
    set_stream(In, newline(detect)),
    set_stream(In, representation_errors(error)),
    set_stream(In, tty(false)),
    message_queue_create(Q),
    da_server_listen(In, Q),
    set_stream(Out, buffer(false)),
    set_stream(Out, tty(false)),
    setup_call_cleanup(Setup,
                       da_server_loop(Out, Q, OnCommand, 1, State),
                       Cleanup).


da_server_loop(_, _, _, stop, _) :- !.
da_server_loop(O, Q, C, Seq0, State0) :-
    thread_get_message(Q, M),
    debug(dap(server), "handling ~w", [M]),
    da_server_handle(O, Q, C, M, Seq0, Seq, State0, State),
    debug(dap(server), "handled ~w", [M]),
    da_server_loop(O, Q, C, Seq, State).

da_server_handle(Out, _, _, action(Action), Seq0, Seq, State, State) :-
    !,
    da_server_perform_action(Out, Action, Seq0, Seq).
da_server_handle(Out, Q, CB, stream(Message), Seq0, Seq, State0, State) :-
    !,
    _{ type : "request",
       seq  : RequestSeq,
       command : Command0
     } :< Message,
    atom_string(Command, Command0),
    (   get_dict(arguments, Message, Arguments)
    ->  true
    ;   Arguments = null
    ),
    debug(dap(server), "Calling cb", []),
    (   call(CB, Command, Arguments, RequestSeq, Q, State0, State)
    ->  Seq = Seq0
    ;   State = State0,
        dap_error(Out, Seq0, RequestSeq, Command, "Bad request"),
        Seq is Seq0 + 1
    ).

da_server_perform_action(Out, response(ReqSeq, Type, Body), Seq0, Seq) :-
    !,
    dap_response(Out, Seq0, ReqSeq, Type, Body),
    Seq is Seq0 + 1.
da_server_perform_action(Out, error(ReqSeq, Type, Body), Seq0, Seq) :-
    !,
    dap_error(Out, Seq0, ReqSeq, Type, Body),
    Seq is Seq0 + 1.
da_server_perform_action(Out, event(Type, Body), Seq0, Seq) :-
    !,
    dap_event(Out, Seq0, Type, Body),
    Seq is Seq0 + 1.
da_server_perform_action(Out, request(Type, Body), Seq0, Seq) :-
    !,
    dap_request(Out, Seq0, Type, Body),
    Seq is Seq0 + 1.
da_server_perform_action(_, stop, _, stop).

da_server_listen(S, Q) :-
    thread_create(da_server_listen_(S, Q), _, []).

da_server_listen_(S, T) :-
    dap_read(S, Message),
    thread_send_message(T, stream(Message)),
    (   get_dict(command, Message, "disconnect")
    ->  true
    ;   da_server_listen_(S, T)
    ).
