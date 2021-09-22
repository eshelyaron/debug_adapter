:- module(
       da_server,
       [
           da_server/1
       ]
   ).

:- use_module(library(http/json)).
:- use_module(tracer).

:- det(da_server/1).
da_server(Options) :-
    option(in(In), Options),
    option(out(Out), Options),
    set_stream(In, buffer(full)),
    set_stream(In, encoding(octet)),
    set_stream(In, newline(dos)),
    set_stream(In, representation_errors(error)),
    set_stream(In, tty(false)),
    set_stream(Out, buffer(false)),
    set_stream(Out, encoding(octet)),
    set_stream(Out, tty(false)),
    pipe(R, W),
    set_stream(R, buffer(full)),
    set_stream(R, encoding(octet)),
    set_stream(W, buffer(false)),
    set_stream(W, alias(w)),
    da_server_loop([], 1, In, Out, R, W).

:- det(da_server_loop/6).
da_server_loop(State0, Seq0, In, Out, R, W) :-
    debug(swipl_dap, "loopin'", []),
    wait_for_input([In, R], Inputs, infinite),
    debug(swipl_dap, "inputs ~w", [Inputs]),
    da_server_handled_streams(In, R, Inputs, Out, W, State0, State, Seq0, Seq),
    da_server_loop(State, Seq, In, Out, R, W).

da_server_handled_streams(In, R, [H|T], Out, W, State0, State, Seq0, Seq) :-
    debug(swipl_dap, "Handling Stream ~w ~w ~w", [In, R, H]),
    da_server_handled_stream(In, R, H, Out, W, State0, State1, Seq0, Seq1),
    da_server_handled_streams(In, R, T, Out, W, State1, State, Seq1, Seq).
da_server_handled_streams(_, _, [], _, _, State, State, Seq, Seq).

da_server_handled_stream(In, _R, In, Out, W, State0, State, Seq0, Seq) :-
    da_server_read_content(In, Content0),
    del_dict(seq,  Content0, ClientSeq, Content1),
    del_dict(type, Content1, Type, Message),
    debug(swipl_dap, "~w, ~w", [ClientSeq, Type]),
    da_server_handled_message(Type, ClientSeq, Message, Out, W, State0, State, Seq0, Seq).
da_server_handled_stream(_In, R, R, Out, _, State0, State, Seq0, Seq) :-
    get_code(R, _),
    debug(swipl_dap, "handle debugge messages", []),
    da_server_handled_debugee_messages(Out, State0, State, Seq0, Seq).

da_server_handled_debugee_messages(Out, State0, State, Seq0, Seq) :-
    debug(swipl_dap, "handling debugge message", []),
    (   thread_peek_message(_)
    ->  thread_get_message(DebugeeThreadId-Message),
        debug(swipl_dap, "handling debugge message ~w ~w", [DebugeeThreadId, Message]),
        da_server_handled_debugee_message(DebugeeThreadId, Message, Out, State0, State1, Seq0, Seq1),
        da_server_handled_debugee_messages(Out, State1, State, Seq1, Seq)
    ;   State = State0,
        Seq   = Seq0
    ).

da_server_handled_debugee_message(_DebugeeThreadId,
                                  loaded_source(Reason, SourcePath),
                                  Out, State, State, Seq0, Seq) :-
    file_base_name(SourcePath, BaseName),
    da_server_emitted_event(Out, Seq0, "loadedSource", _{ reason : Reason,
                                                          source : _{ name : BaseName,
                                                                      path : SourcePath
                                                                    }
                                                        }
                           ),
    succ(Seq0, Seq).
da_server_handled_debugee_message(DebugeeThreadId,
                                  stopped(Reason, Description, Text, BreakpointIds),
                                  Out, State, State, Seq0, Seq) :-
    da_server_emitted_event(Out, Seq0, "stopped", _{ threadId         : DebugeeThreadId,
                                                     reason           : Reason,
                                                     description      : Description,
                                                     text             : Text,
                                                     hitBreakpointIds : BreakpointIds
                                                   }
                           ),
    succ(Seq0, Seq).
da_server_handled_debugee_message(_DebugeeThreadId,
                                  stack_trace(RequestSeq, StackFrames0),
                                  Out, State, State, Seq0, Seq) :-
    maplist(prolog_dap_stack_frame, StackFrames0, StackFrames),
    da_server_emitted_response(Out, Seq0, RequestSeq, "stackTrace", _{stackFrames:StackFrames}),
    succ(Seq0, Seq).

prolog_dap_stack_frame(stack_frame(Id, Name, 0, null, _, _, _, _),  % foreign predicate
                       _{ id     : Id,
                          name   : Name,
                          line   : 0,
                          column : 0
                        }
                      ) :- !.
prolog_dap_stack_frame(stack_frame(Id, Name, Ref, null, _, _, _, _),
                       _{ id     : Id,
                          name   : Name,
                          line   : 0,
                          column : 0,
                          source : _{ name            : Name,
                                      sourceReference : Ref,
                                      origin          : "Dynamic"
                                    }
                        }
                      ) :- !.
prolog_dap_stack_frame(stack_frame(Id, Name, Ref, Path, SL, SC, EL, EC),
                       _{ id        : Id,
                          name      : Name,
                          line      : SL,
                          column    : SC,
                          endLine   : EL,
                          endColumn : EC,
                          source    : _{ name            : BaseName,
                                         path            : Path,
                                         sourceReference : Ref,
                                         origin          : "Static"
                                       }
                        }
                      ) :-
    file_base_name(Path, BaseName).

:- det(da_server_read_content/2).
da_server_read_content(In, Content) :-
    read_line_to_string(In, Line),
    sub_string(Line, 16, _, 0, ContentLengthString), % string_length("Content-Length: ", 16).
    number_string(ContentLength, ContentLengthString),
    read_line_to_string(In, ""),
    read_string(In, ContentLength, Serialized),
    atom_json_dict(Serialized, Content, []).

da_server_handled_message("request", RequestSeq, Message0, Out, W, State0, State, Seq0, Seq) :-
    del_dict(command, Message0, Command, Message),
    debug(swipl_dap, "~w", Command),
    da_server_command(Command, RequestSeq, Message, Out, W, State0, State, Seq0, Seq).

da_server_command("initialize", RequestSeq, Message, Out, _W, State, State, Seq0, Seq) :-
    _{ arguments:Args } :< Message,
    da_server_capabilities(Capabilities),
    da_server_emitted_response(Out, Seq0, RequestSeq, "initialize", Capabilities),
    succ(Seq0, Seq1),
    da_initialized(Args),
    da_server_emitted_event(Out, Seq1, "initialized"),
    succ(Seq1, Seq).
da_server_command("launch", RequestSeq, Message, Out, W, State, [Debugee|State], Seq0, Seq) :-
    _{ arguments:Args } :< Message,
    debug(swipl_dap, "launch args ~w", [Args]),
    da_launched(Args, W, Debugee),
    debug(swipl_dap, "launched ~w", [Debugee]),
    da_server_emitted_response(Out, Seq0, RequestSeq, "launch"),
    succ(Seq0, Seq),
    debug(swipl_dap, "launch unifying", []).
da_server_command("configurationDone", RequestSeq, _Message, Out, _W, State, State, Seq0, Seq) :-
    da_configured(State),
    da_server_emitted_response(Out, Seq0, RequestSeq, "configurationDone"),
    succ(Seq0, Seq).
da_server_command("threads", RequestSeq, _Message, Out, _W, State, State, Seq0, Seq) :-
    maplist(prolog_dap_thread, State, Threads),
    da_server_emitted_response(Out, Seq0, RequestSeq, "threads", _{threads:Threads}),
    succ(Seq0, Seq).
da_server_command("stackTrace", RequestSeq, Message, _Out, _W, State, State, Seq, Seq) :-
    _{ arguments:Args } :< Message,
    _{ threadId:ThreadId } :< Args,
    thread_send_message(ThreadId, stack_trace(RequestSeq)).
da_server_command("stepIn", RequestSeq, Message, Out, _W, State, State, Seq0, Seq) :-
    _{ arguments:Args } :< Message,
    _{ threadId:ThreadId } :< Args,
    da_server_emitted_response(Out, Seq0, RequestSeq, "stepIn"),
    succ(Seq0, Seq),
    thread_send_message(ThreadId, step_in).

prolog_dap_thread(debugee(PrologThreadId, ThreadId, _),
                  _{ name : Name,
                     id   : ThreadId
                   }
                 ) :-
    (   thread_property(PrologThreadId, alias(Name0)), !
    ;   thread_property(PrologThreadId, id(Name0))
    ),
    term_string(Name0, Name).

da_server_capabilities(_{supportsConfigurationDoneRequest: true}).

da_initialized(_).

da_launched(Args, W, debugee(PrologThreadId, ThreadId, Goal)) :-
    _{ module: ModulePath, goal: GoalString } :< Args,
    term_string(Goal, GoalString),
    debug(swipl_dap, "launching goal ~w", [Goal]),
    thread_self(ServerThreadId),
    thread_create(da_debugee(ModulePath, Goal, ServerThreadId, W), PrologThreadId),
    debug(swipl_dap, "created thread ~w", [PrologThreadId]),
    thread_property(PrologThreadId, id(ThreadId)),
    debug(swipl_dap, "created thread s ~w", [ThreadId]).

da_configured([debugee(_, ThreadId, _)|T]) :-
    thread_send_message(ThreadId, configuration_done),
    da_configured(T).
da_configured([]).

da_server_emitted_response(Out, Seq, RequestSeq, Command) :-
    da_server_emitted_response(Out, Seq, RequestSeq, Command, null).

da_server_emitted_response(Out, Seq, RequestSeq, Command, Body) :-
    da_server_emitted_response(Out, Seq, RequestSeq, Command, true, null, Body).

da_server_emitted_response(Out, Seq, RequestSeq, Command, Success, Message, Body) :-
    da_server_emitted_message(Out, Seq, "response", _{ request_seq: RequestSeq,
                                                       success    : Success,
                                                       command    : Command,
                                                       message    : Message,
                                                       body       : Body
                                                     }).

da_server_emitted_event(Out, Seq, Event) :-
    da_server_emitted_event(Out, Seq, Event, null).

da_server_emitted_event(Out, Seq, Event, Body) :-
    da_server_emitted_message(Out, Seq, "event", _{ event: Event,
                                                    body : Body
                                                  }
                             ).

da_server_emitted_error(Out, Seq, RequestSeq, Command, Message) :-
    da_server_emitted_response(Out, Seq, RequestSeq, Command, false, Message, null).

da_server_emitted_message(Out, Seq, Type, Rest) :-
    put_dict(_{ seq : Seq,
                type: Type
              },
             Rest,
             Message
            ),
    atom_json_dict(Serialized, Message, []),
    da_server_emitted_serialized_content(Out, Serialized).

da_server_emitted_serialized_content(Out, Content) :-
    string_length(Content, ContentLength),
    format(Out, "Content-Length: ~w\r\n\r\n", [ContentLength]),
    format(Out, "~w", Content).
