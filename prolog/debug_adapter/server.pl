:- module(
       da_server,
       [
           da_server/1
       ]
   ).

:- predicate_options(da_server/1, 1, [ in(+stream),
                                       out(+stream)
                                     ]
                    ).

/** <module> SWI-Prolog Debug Adapter Server

This module contains the core logic for handling DAP request sent by DAP clients which are most
commonly IDE extensions controlled interactivly by a progammer. The main entry point is da_server/1.

The implementation is most dominently guided by the [DAP
specification](https://microsoft.github.io/debug-adapter-protocol/specification).
*/

:- use_module(tracer).
:- use_module(protocol).

%!  da_server(+Options) is det.
%
%   Starts the DAP server in the current thread. Options:
%    - in(+Stream)
%      Required. Stream will be used by the server to read incoming messages from the client.
%    - out(+Stream)
%      Required. Stream will be used by the server to emit outgoing messages to the client.

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
    da_server_loop([], 1, In, Out, R, W).

:- det(da_server_loop/6).
da_server_loop([], exiting(Seq), _In, Out, R, W) :-
    !,
    close(R),
    close(W),
    dap_event(Out, Seq, "exited", _{exitCode:0}),
    thread_exit(0).
da_server_loop(State0, exiting(Seq0), In, Out, R, W) :-
    !,
    wait_for_input([R], Inputs, infinite),
    da_server_exiting_handled_stream(Inputs, Out, W, State0, State, Seq0, Seq),
    da_server_loop(State, exiting(Seq), In, Out, R, W).
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

da_server_exiting_handled_stream([R], Out, _, State0, State, Seq0, Seq) :-
    get_code(R, _),
    da_server_exiting_handled_debugee_messages(Out, State0, State, Seq0, Seq).

da_server_exiting_handled_debugee_messages(Out, State0, State, Seq0, Seq) :-
    (   thread_peek_message(_)
    ->  thread_get_message(DebugeeThreadId-Message),
        da_server_exiting_handled_debugee_message(DebugeeThreadId, Message, Out, State0, State1, Seq0, Seq1),
        da_server_exiting_handled_debugee_messages(Out, State1, State, Seq1, Seq)
    ;   State = State0,
        Seq   = Seq0
    ).

da_server_exiting_handled_debugee_message(DebugeeThreadId, Message,
                                          Out, State0, State, Seq0, Seq) :-
    (   functor(Message, stopped, _)
    ->  catch(thread_send_message(DebugeeThreadId, disconnect), _, true)
    ;   true
    ),
    da_server_handled_debugee_message(DebugeeThreadId, Message, Out, State0, State, Seq0, Seq).

da_server_handled_debugee_messages(Out, State0, State, Seq0, Seq) :-
    (   thread_peek_message(_)
    ->  thread_get_message(DebugeeThreadId-Message),
        debug(dap(server), "Received message ~w from debugee thread ~w", [Message, DebugeeThreadId]),
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
                                  scopes(RequestSeq, Scopes0),
                                  Out, State, State, Seq0, Seq) :-
    maplist(prolog_dap_scope, Scopes0, Scopes),
    dap_response(Out, Seq0, RequestSeq, "scopes", _{scopes:Scopes}),
    succ(Seq0, Seq).
da_server_handled_debugee_message(_DebugeeThreadId,
                                  variables(RequestSeq, Variables0),
                                  Out, State, State, Seq0, Seq) :-
    maplist(prolog_dap_variable, Variables0, Variables),
    dap_response(Out, Seq0, RequestSeq, "variables", _{variables:Variables}),
    succ(Seq0, Seq).
da_server_handled_debugee_message(_DebugeeThreadId,
                                  exited(_ExitCode),
                                  _Out, State, State, Seq, Seq).
%    dap_event(Out, Seq0, "exited", _{exitCode:ExitCode}),
%    succ(Seq0, Seq).
da_server_handled_debugee_message(DebugeeThreadId,
                                  thread_exited,
                                  Out, State0, State, Seq0, Seq) :-
    dap_event(Out, Seq0, "thread", _{ reason   : "exited",
                                      threadId : DebugeeThreadId
                                    }),
    succ(Seq0, Seq),
    select(debugee(_, DebugeeThreadId, _), State0, State).


prolog_dap_scope(scope(Name, VariablesRef, SourceSpan),
                 _{ name               : Name,
                    variablesReference : VariablesRef,
                    expensive          : false,
                    source             : DAPSource,
                    line               : SL,
                    column             : SC,
                    endLine            : EL,
                    endColumn          : EC
                  }
                ) :-
    prolog_dap_source_span(SourceSpan, DAPSource, SL, SC, EL, EC).

prolog_dap_variable(variable(Name, Value, VariablesRef),
                    _{ name               : Name,
                       variablesReference : VariablesRef,
                       value              : Value
                     }
                   ) :- !.

prolog_dap_source_span(span(File, SL, SC, EL, EC),
                       _{ name            : Name,
                          path            : File,
                          origin          : "Static"
                        },
                       SL, SC, EL, EC
                      ) :-
    !,
    file_base_name(File, Name).
prolog_dap_source_span(reference(SourceReference),
                       _{ name            : "*dynamic*",
                          sourceReference : SourceReference,
                          origin          : "Dynamic"
                        },
                       0, 0, null, null
                      ).

prolog_dap_stack_frame(stack_frame(Id, PI, _Alternative, SourceSpan),
                       _{ id          : Id,
                          name        : Name,
                          line        : SL,
                          column      : SC,
                          endLine     : EL,
                          endColumn   : EC,
                          source      : DAPSource
%                         alternative : DAPAlternative  experiment with a custom protocol extension
                        }
                      ) :-
%   prolog_dap_alternative(Alternative, DAPAlternative),
    term_string(PI, Name),
    prolog_dap_source_span(SourceSpan, DAPSource, SL, SC, EL, EC).


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
da_server_command("continue", RequestSeq, Message, Out, _W, State, State, Seq0, Seq) :-
    _{ arguments:Args } :< Message,
    _{ threadId:ThreadId } :< Args,
    dap_response(Out, Seq0, RequestSeq, "continue", _{allThreadsContinued:null}),
    succ(Seq0, Seq),
    thread_send_message(ThreadId, continue).
da_server_command("disconnect", RequestSeq, _Message, Out, _W, State, State, Seq0, exiting(Seq)) :-
    maplist(da_server_disconnect_debugee, State),
    dap_response(Out, Seq0, RequestSeq, "disconnect"),
    succ(Seq0, Seq).
da_server_command("restartFrame", RequestSeq, Message, Out, _W, State, State, Seq0, Seq) :-
    _{ arguments : Args    } :< Message,
    _{ frameId   : FrameId } :< Args,
    dap_response(Out, Seq0, RequestSeq, "restartFrame"),
    succ(Seq0, Seq),
    maplist([debugee(PrologThreadId, _, _)]>>thread_send_message(PrologThreadId, restart_frame(FrameId)), State).
da_server_command("scopes", RequestSeq, Message, _Out, _W, State, State, Seq, Seq) :-
    _{ arguments:Args } :< Message,
    _{ frameId:FrameId } :< Args,
    maplist({RequestSeq, FrameId}/[debugee(PrologThreadId, _, _)]>>thread_send_message(PrologThreadId, scopes(RequestSeq, FrameId)), State).
da_server_command("variables", RequestSeq, Message, _Out, _W, State, State, Seq, Seq) :-
    _{ arguments:Args } :< Message,
    _{ variablesReference:VariablesRef } :< Args,
    maplist({RequestSeq, VariablesRef}/[debugee(PrologThreadId, _, _)]>>thread_send_message(PrologThreadId, variables(RequestSeq, VariablesRef)), State).
da_server_command(Command, RequestSeq, _Message, Out, _W, State, State, Seq0, Seq) :-
    format(string(ErrorMessage), "Command \"~w\" is not implemented", [Command]),
    dap_error(Out, Seq0, RequestSeq, Command, ErrorMessage),
    succ(Seq0, Seq).


da_server_disconnect_debugee(debugee(BlobThreadId, _EphermalThreadId, _Goal)) :-
    thread_send_message(BlobThreadId, disconnect).

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
    _{ cwd: CWD, module: ModulePath, goal: GoalString } :< Args,
    cd(CWD),
    term_string(Goal, GoalString),
    thread_self(ServerThreadId),
    thread_create(da_debugee(ModulePath, Goal, ServerThreadId, W), PrologThreadId),
    thread_property(PrologThreadId, id(ThreadId)).

da_configured([debugee(_, ThreadId, _)|T]) :-
    thread_send_message(ThreadId, configuration_done),
    da_configured(T).
da_configured([]).
