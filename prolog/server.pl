:- module(
       da_server,
       [
           da_server/1
       ]
   ).

:- use_module(tracer).
:- use_module(protocol).

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
    wait_for_input([In, R], Inputs, infinite),
    da_server_handled_streams(In, R, Inputs, Out, W, State0, State, Seq0, Seq),
    da_server_loop(State, Seq, In, Out, R, W).

da_server_handled_streams(In, R, [H|T], Out, W, State0, State, Seq0, Seq) :-
    da_server_handled_stream(In, R, H, Out, W, State0, State1, Seq0, Seq1),
    da_server_handled_streams(In, R, T, Out, W, State1, State, Seq1, Seq).
da_server_handled_streams(_, _, [], _, _, State, State, Seq, Seq).

da_server_handled_stream(In, _R, In, Out, W, State0, State, Seq0, Seq) :-
    dap_read(In, Message0),
    del_dict(seq,  Message0, ClientSeq, Message1),
    del_dict(type, Message1, Type, Message),
    da_server_handled_message(Type, ClientSeq, Message, Out, W, State0, State, Seq0, Seq).
da_server_handled_stream(_In, R, R, Out, _, State0, State, Seq0, Seq) :-
    get_code(R, _),
    da_server_handled_debugee_messages(Out, State0, State, Seq0, Seq).

da_server_handled_debugee_messages(Out, State0, State, Seq0, Seq) :-
    (   thread_peek_message(_)
    ->  thread_get_message(DebugeeThreadId-Message),
        da_server_handled_debugee_message(DebugeeThreadId, Message, Out, State0, State1, Seq0, Seq1),
        da_server_handled_debugee_messages(Out, State1, State, Seq1, Seq)
    ;   State = State0,
        Seq   = Seq0
    ).

da_server_handled_debugee_message(_DebugeeThreadId,
                                  loaded_source(Reason, SourcePath),
                                  Out, State, State, Seq0, Seq) :-
    file_base_name(SourcePath, BaseName),
    dap_event(Out, Seq0, "loadedSource", _{ reason : Reason,
                                                          source : _{ name : BaseName,
                                                                      path : SourcePath
                                                                    }
                                                        }
                           ),
    succ(Seq0, Seq).
da_server_handled_debugee_message(DebugeeThreadId,
                                  stopped(Reason, Description, Text, BreakpointIds),
                                  Out, State, State, Seq0, Seq) :-
    dap_event(Out, Seq0, "stopped", _{ threadId         : DebugeeThreadId,
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
    dap_response(Out, Seq0, RequestSeq, "stackTrace", _{stackFrames:StackFrames}),
    succ(Seq0, Seq).
da_server_handled_debugee_message(_DebugeeThreadId,
                                  exited(ExitCode),
                                  Out, State, State, Seq0, Seq) :-
    dap_event(Out, Seq0, "exited", _{exitCode:ExitCode}),
    succ(Seq0, Seq).
da_server_handled_debugee_message(DebugeeThreadId,
                                  thread_exited,
                                  Out, State, State, Seq0, Seq) :-
    dap_event(Out, Seq0, "thread", _{ reason   : "exited",
                                                    threadId : DebugeeThreadId
                                                  }),
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

da_server_handled_message("request", RequestSeq, Message0, Out, W, State0, State, Seq0, Seq) :-
    del_dict(command, Message0, Command, Message),
    da_server_command(Command, RequestSeq, Message, Out, W, State0, State, Seq0, Seq).

da_server_command("initialize", RequestSeq, Message, Out, _W, State, State, Seq0, Seq) :-
    _{ arguments:Args } :< Message,
    da_server_capabilities(Capabilities),
    dap_response(Out, Seq0, RequestSeq, "initialize", Capabilities),
    succ(Seq0, Seq1),
    da_initialized(Args),
    dap_event(Out, Seq1, "initialized"),
    succ(Seq1, Seq).
da_server_command("launch", RequestSeq, Message, Out, W, State, [Debugee|State], Seq0, Seq) :-
    _{ arguments:Args } :< Message,
    da_launched(Args, W, Debugee),
    dap_response(Out, Seq0, RequestSeq, "launch"),
    succ(Seq0, Seq).
da_server_command("configurationDone", RequestSeq, _Message, Out, _W, State, State, Seq0, Seq) :-
    da_configured(State),
    dap_response(Out, Seq0, RequestSeq, "configurationDone"),
    succ(Seq0, Seq).
da_server_command("threads", RequestSeq, _Message, Out, _W, State, State, Seq0, Seq) :-
    maplist(prolog_dap_thread, State, Threads),
    dap_response(Out, Seq0, RequestSeq, "threads", _{threads:Threads}),
    succ(Seq0, Seq).
da_server_command("stackTrace", RequestSeq, Message, Out, _W, State, State, Seq0, Seq) :-
    _{ arguments:Args } :< Message,
    _{ threadId:ThreadId } :< Args,
    catch((thread_send_message(ThreadId, stack_trace(RequestSeq)), Seq = Seq0),
          _Catcher,
          (dap_error(Out, Seq0, RequestSeq, "stackTrace", null), succ(Seq0, Seq))
         ).
da_server_command("stepIn", RequestSeq, Message, Out, _W, State, State, Seq0, Seq) :-
    _{ arguments:Args } :< Message,
    _{ threadId:ThreadId } :< Args,
    dap_response(Out, Seq0, RequestSeq, "stepIn"),
    succ(Seq0, Seq),
    thread_send_message(ThreadId, step_in).
da_server_command("disconnect", RequestSeq, _Message, Out, _W, State, State, Seq0, Seq) :-
    dap_response(Out, Seq0, RequestSeq, "disconnect"),
    succ(Seq0, Seq),
    halt.

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
    thread_self(ServerThreadId),
    thread_create(da_debugee(ModulePath, Goal, ServerThreadId, W), PrologThreadId),
    thread_property(PrologThreadId, id(ThreadId)).

da_configured([debugee(_, ThreadId, _)|T]) :-
    thread_send_message(ThreadId, configuration_done),
    da_configured(T).
da_configured([]).
