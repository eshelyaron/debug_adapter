:- module(
       dap_server,
       [
           dap_server/1
       ]
   ).

:- use_module(library(http/json)).
:- use_module(library(record)).
:- use_module(dap_debugee).
:- use_module(dap_tracer).

:- record dap_server_state(status:atom=started,
                           seq:positive_integer=1,
                           debugee
                          ).
:- det(dap_server/1).
dap_server(Options) :-
    option(in(In), Options),
    option(out(Out), Options),
    set_stream(In, buffer(full)),
    set_stream(In, encoding(octet)),
    set_stream(In, newline(dos)),
    set_stream(In, representation_errors(error)),
    set_stream(In, tty(false)),
    set_stream(In, alias(dap_server_in)),
    set_stream(Out, buffer(false)),
    set_stream(Out, encoding(octet)),
    set_stream(Out, tty(false)),
    set_stream(Out, alias(dap_server_out)),
    pipe(R, W),
    set_stream(R, buffer(full)),
    set_stream(R, encoding(octet)),
    set_stream(R, alias(dap_server_debugee_in)),
    set_stream(W, buffer(false)),
    set_stream(W, alias(dap_server_debugee_out)),
    message_queue_create(dap_server_debugee_queue),
    default_dap_server_state(S),
    dap_server_loop(S).

:- det(dap_server_loop/1).
dap_server_loop(S0) :-
    wait_for_input([dap_server_in, dap_server_debugee_in], Inputs, infinite),
    dap_server_process_streams(Inputs, S0, S),
    dap_server_loop(S).

:- det(dap_server_process_streams/3).
dap_server_process_streams([H|T], S0, S) :-
    dap_server_process_stream(H, S0, S1),
    dap_server_process_streams(T, S1, S).
dap_server_process_streams([], S, S).

:- det(dap_server_process_stream/3).
dap_server_process_stream(dap_server_in, S0, S) :-
    dap_server_read_message(dap_server_in, Message),
    dap_server_handled_message(Message, S0, S).
dap_server_process_stream(dap_server_debugee_in, S0, S) :-
    get_code(dap_server_debugee_in, Code),
    (   Code = 0
    ->  dap_server_handled_debugee_success(S0, S)
    ;   Code = 1
    ->  dap_server_handled_debugee_failure(S0, S)
    ;   Code = 2
    ->  dap_server_handled_debugee_error(S0, S)
    ;   Code = 3
    ->  dap_server_handled_debugee_queue(S0, S)
    ).

dap_server_handled_debugee_success(S0, S) :-
    dap_server_state_seq_inceremented(ServerSeq, S0, S1),
    dap_event(dap_server_out, ServerSeq, "exited", _{exitCode:0}),
    set_status_of_dap_server_state(exited, S1, S).

dap_server_handled_debugee_failure(S0, S) :-
    dap_server_state_seq_inceremented(ServerSeq, S0, S1),
    dap_event(dap_server_out, ServerSeq, "exited", _{exitCode:1}),
    set_status_of_dap_server_state(exited, S1, S).

dap_server_handled_debugee_error(S0, S) :-
    dap_server_state_seq_inceremented(ServerSeq, S0, S1),
    dap_event(dap_server_out, ServerSeq, "exited", _{exitCode:2}),
    set_status_of_dap_server_state(exited, S1, S).

dap_server_handled_debugee_queue(S0, S) :-
    thread_get_message(dap_server_debugee_queue, intercepting(DebugeeThreadId, Port, Frame, Choice)),
    dap_prolog_stopped_reason(Port, Frame, Choice, Reason, Description, S0, S1),
    thread_property(DebugeeThreadId, system_thread_id(TheadId)),
    dap_server_state_seq_inceremented(ServerSeq, S1, S),
    dap_event(dap_server_out, ServerSeq, "stopped", _{reason:Reason, description:Description, threadId:TheadId}).


dap_prolog_stopped_reason(_Port, _Frame, _Choice, Reason, Description, S0, S) :-
    dap_server_state_status(S0, Status),
    (   Status = entry
    ->  Reason = "entry", Description = "Entering debugee", S = S0
    ).

:- det(dap_server_read_message/2).
dap_server_read_message(In, Message) :-
    read_line_to_string(In, Line),
    sub_string(Line, 16, _, 0, ContentLengthString), % string_length("Content-Length: ", 16).
    number_string(ContentLength, ContentLengthString),
    read_line_to_string(In, ""),
    read_string(In, ContentLength, JsonString),
    atom_json_dict(JsonString, Message, []).

dap_server_handled_message(Message, S0, S) :-
    _{ type:Type } :< Message,
    (   Type = "request"
    ->  dap_server_handled_request(Message, S0, S)
    ).

dap_server_handled_request(Message, S0, S) :-
    _{ command:Command } :< Message,
    dap_server_state_status(S0, Status),
    dap_handle_command(Status, Command, Message, S0, S).

dap_handle_command(started, "initialize", Message, S0, S) :-
    _{ seq:Seq, arguments:Args } :< Message,
    dap_capabilities(Args, Capabilities),
    dap_server_state_seq_inceremented(ServerSeq0, S0, S1),
    dap_response(dap_server_out, ServerSeq0, Seq, "initialize", Capabilities),
    dap_initialized_with_arguments(Args, S1, S).
dap_handle_command(initialized, "launch", Message, S0, S) :-
    _{ seq:Seq, arguments:Args } :< Message,
    dap_launched_debugee_with_arguments(Args, S0, S1),
    dap_server_state_seq_inceremented(ServerSeq, S1, S),
    dap_response(dap_server_out, ServerSeq, Seq, "launch").
dap_handle_command(ready, "configurationDone", Message, S0, S) :-
    _{ seq:Seq } :< Message,
    dap_server_state_seq_inceremented(ServerSeq, S0, S1),
    dap_response(dap_server_out, ServerSeq, Seq, "configurationDone"),
    dap_configured(S1, S).

dap_capabilities(_, _{supportsConfigurationDoneRequest:true}).

dap_initialized_with_arguments(_Args, S0, S) :-
    dap_server_state_seq_inceremented(ServerSeq, S0, S1),
    dap_event(dap_server_out, ServerSeq, "initialized"),
    set_status_of_dap_server_state(initialized, S1, S).

dap_launched_debugee_with_arguments(Args, S0, S) :-
    _{ module: ModulePathString, goal: GoalString } :< Args,
    term_string(Goal, GoalString),
    atom_string(ModulePathAtom, ModulePathString),
    use_module(ModulePathAtom),
    module_property(ModuleAtom, file(ModulePathAtom)),
    atom_string(ModuleAtom, ModuleString),
    dap_server_state_seq_inceremented(ServerSeq, S0, S1),
    dap_event(dap_server_out, ServerSeq, "loadedSource",  _{reason:"new", source:_{name:ModuleString, path:ModulePathString}}),
    thread_create(dap_debugee(Goal), DebugeeThreadId, [debug(true)]),
    set_debugee_of_dap_server_state(DebugeeThreadId, S1, S2),
    set_status_of_dap_server_state(ready, S2, S).

dap_configured(S0, S) :-
    asserta(( user:prolog_trace_interception(Port, Frame, Choice, Action) :-
                  dap_trace_interception(Port, Frame, Choice, Action)
            )
           ),
    dap_server_state_debugee(S0, DebugeeThreadId),
    thread_send_message(DebugeeThreadId, go),  % arbitrary term
    set_status_of_dap_server_state(entry, S0, S).

dap_error_response(Out, ServerSeq, Seq, Command, Message) :-
    atom_json_dict(JsonString, _{seq:ServerSeq,
                                 type:"response",
                                 request_seq:Seq,
                                 success:false,
                                 command:Command,
                                 message:Message,
                                 body:_{}
                                }, []),
    debug(swipl_dap, "swipl_dap: Request[~w]: Response[~w]: Error: ~w", [Seq, ServerSeq, Message]),
    dap_send(Out, JsonString).

dap_response(Out, ServerSeq, Seq, Command, Success, Message, Body) :-
    atom_json_dict(JsonString, _{seq:ServerSeq,
                                 type:"response",
                                 request_seq:Seq,
                                 success:Success,
                                 command:Command,
                                 message:Message,
                                 body:Body
                                }, []),
    debug(swipl_dap, "Response ID: ~w", [ServerSeq]),
    dap_send(Out, JsonString).

dap_response(Out, ServerSeq, Seq, Command, Body) :-
    atom_json_dict(JsonString, _{seq:ServerSeq,
                                 type:"response",
                                 request_seq:Seq,
                                 success:true,
                                 command:Command,
                                 body:Body
                                }, []),
    debug(swipl_dap, "Response ID: ~w", [ServerSeq]),
    dap_send(Out, JsonString).

dap_response(Out, ServerSeq, Seq, Command) :-
    atom_json_dict(JsonString, _{seq:ServerSeq,
                                 type:"response",
                                 request_seq:Seq,
                                 success:true,
                                 command:Command
                                }, []),
    debug(swipl_dap, "Response ID: ~w", [ServerSeq]),
    dap_send(Out, JsonString).

dap_event(Out, ServerSeq, Event) :-
    atom_json_dict(JsonString, _{seq:ServerSeq,
                                 type:"event",
                                 event:Event
                                }, []),
    debug(swipl_dap, "swipl_dap: Event[~w]: ~w", [ServerSeq, Event]),
    dap_send(Out, JsonString).

dap_event(Out, ServerSeq, Event, Body) :-
    atom_json_dict(JsonString, _{seq:ServerSeq,
                                 type:"event",
                                 event:Event,
                                 body:Body
                                }, []),
    debug(swipl_dap, "swipl_dap: Event[~w]: ~w: ~w", [ServerSeq, Event, Body]),
    dap_send(Out, JsonString).

dap_send(Out, JsonString) :-
    string_length(JsonString, JsonStringLength),
    debug(swipl_dap, "swipl_dap: Message Content Length: ~w", [JsonStringLength]),
    format(Out, "Content-Length: ~w\r\n\r\n", [JsonStringLength]),
    debug(swipl_dap, "swipl_dap: Message Content: ~w", [JsonString]),
    format(Out, "~w", JsonString).

dap_server_state_seq_inceremented(Old, S0, S) :-
    dap_server_state_seq(S0, Old),
    succ(Old, New),
    set_seq_of_dap_server_state(New, S0, S).
