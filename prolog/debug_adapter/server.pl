:- module(
       da_server,
       [
           da_server/1
       ]
   ).


/** <module> SWI-Prolog Debug Adapter Protocol Server

This module contains the core logic for handling DAP requests sent by DAP clients which are most
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
                                       threads(+list(pair)),
                                       setup(+callable),
                                       on_command(+callable),
                                       cleanup(+callable),
                                       initial_state(+any)
                                     ]
                    ).


%!  da_server(+Options) is det.
%
%   Starts the DAP server in the current thread. Options:
%    * in(+Stream)
%      Stream will be used by the server to read incoming messages from the client. Defaults to =user_input=.
%    * out(+Stream)
%      Stream will be used by the server to emit outgoing messages to the client. Defaults to =user_output=.
%    * setup(:Setup)
%      Setup will be called just before entering the main DAP server loop. Defaults to =true=.
%    * on_command(:OnCommand)
%      OnCommand is a closure of arity 6 which will be called during the DAP session loop
%      to handle incoming DAP request, like so:
%      ```
%      call(OnCommand, +Command, +Arguments, +RequestSeq, +Handle, +State0, -State)
%      ```
%      Where:
%      - _Command_ is an atom identyfing the type of the DAP request, e.g. =stepIn=.
%      - _Arguments_ is either the atom =null= or a dict containing _Command_ -specific parameters.
%      - _RequestSeq_ is an integer identifying the received request in the scope of the current session.
%      - _Handle_ can be used to with the predicate from module =da_sdk= to
%        communicate DAP messages (including the response for the handled command) back to the client.
%      - _State0_ and _State_ can be used to pass arbitrary terms between invocations of _OnCommand_
%        during the course of a DAP session. The session loop will initially call _OnCommand_ with
%        _State0_ bound to an initial state term determined by the =initial_state= option of this
%        predicate, in the next invocation _State0_ will be bound to the _State_ argument of the prior
%        invocation, and so forth.
%    * cleanup(:Cleanup)
%      Cleanup will be called after the main DAP server loop completes. Defaults to =true=.
%    * initial_state(+State)
%      State will be used as the initial state argument passed to _OnCommand_. Defaults to [].

:- det(da_server/1).
da_server(Options) :-
    option(in(In), Options, user_input),
    option(out(Out), Options, user_output),
    option(setup(Setup), Options, true),
    option(on_command(OnCommand), Options),
    option(cleanup(Cleanup), Options, true),
    option(initial_state(State), Options, []),
    get_time(Now),
    option(timeout(Timeout), Options, 3600),
    Deadline is Now + Timeout,
    set_stream(In, buffer(full)),
    set_stream(In, newline(detect)),
    set_stream(In, representation_errors(error)),
    set_stream(In, tty(false)),
    message_queue_create(Q),
    da_server_listen(In, Q),
    set_stream(Out, buffer(false)),
    set_stream(Out, tty(false)),
    setup_call_cleanup(Setup,
                       da_server_loop(Out, Q, Deadline, OnCommand, 1, State),
                       Cleanup).


da_server_loop(_, _, _, _, stop, _) :- !.
da_server_loop(O, Q, Deadline, C, Seq0, State0) :-
    thread_get_message(Q, M, [deadline(Deadline)]),
    debug(dap(server), "handling ~w", [M]),
    da_server_handle(O, Q, C, M, Seq0, Seq, State0, State),
    debug(dap(server), "handled ~w", [M]),
    da_server_loop(O, Q, Deadline, C, Seq, State).

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
