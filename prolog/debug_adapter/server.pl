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
:- use_module(tracer).
:- use_module(protocol).
:- use_module(source).
:- use_module(clause).
:- use_module(breakpoint).


:- predicate_options(da_server/1, 1, [ in(+stream),
                                       out(+stream),
                                       threads(+list(pair))
                                     ]
                    ).


%!  da_server(+Options) is det.
%
%   Starts the DAP server in the current thread. Options:
%    - in(+Stream)
%      Stream will be used by the server to read incoming messages from the client. Defaults to user_input.
%    - out(+Stream)
%      Stream will be used by the server to emit outgoing messages to the client. Defaults to user_output.
%    - threads(+Threads)
%      List of initial debuggee threads to be monitored by the server.


:- det(da_server/1).
da_server(Options) :-
    option(in(In), Options, user_input),
    option(out(Out), Options, user_output),
    set_stream(In, buffer(full)),
    set_stream(In, newline(detect)),
    set_stream(In, representation_errors(error)),
    set_stream(In, tty(false)),
    thread_self(Self),
    thread_create(stream_forward(In, Self), _, []),
    set_stream(Out, buffer(false)),
    set_stream(Out, tty(false)),
    option(threads(Threads), Options, []),
    forall(member(T-S, Threads), asserta(da_server_debugee_thread(T, S))),
    da_server_loop(1, Out).

:- thread_local da_server_debugee_thread/2.
:- thread_local da_server_disconnecting/0.

:- det(da_server_loop/2).
da_server_loop(Seq0, Out) :-
    debug(dap(server), "waiting for a message", []),
    thread_get_message(M),
    da_server_handle(M, Seq0, Seq, Out),
    debug(dap(server), "handled ~w", [M]),
    da_server_loop(Seq, Out).

da_server_handle(DebugeeThreadId-Message, Seq0, Seq, Out) :-
    !,
    debug(dap(server), "Received message ~w from debugee thread ~w", [Message, DebugeeThreadId]),
    da_server_handle_debugee_message(DebugeeThreadId, Message, Out, Seq0, Seq),
    debug(dap(server), "Handled message ~w from debugee thread ~w", [Message, DebugeeThreadId]).
da_server_handle(stream(_, _), Seq, Seq, _) :-
    da_server_disconnecting, !.
da_server_handle(stream(_, Message), Seq, Seq, _) :-
    get_dict(type, Message, "response"), !.
da_server_handle(stream(_, Message0), Seq0, Seq, Out) :-
    !,
    del_dict(seq, Message0, RequestSeq, Message1),
    del_dict(command, Message1, Command, Message),
    da_server_command(Command, RequestSeq, Message, Out, Seq0, Seq).


stream_forward(S, T) :-
    debug(dap(server), "waiting", []),
    dap_read(S, Message),
    debug(dap(server), "forwarding ~w to ~w", [Message, T]),
    thread_send_message(T, stream(S, Message)),
    (   get_dict(command, Message, "disconnect")
    ->  debug(dap(server), "bye", [])
    ;   stream_forward(S, T)
    ).


:- det(da_server_handle_debugee_message/5).
da_server_handle_debugee_message(_DebugeeThreadId,
                                 loaded_source(Reason, SourcePath),
                                 Out, Seq0, Seq) :-
    !,
    file_base_name(SourcePath, BaseName),
    dap_event(Out, Seq0, "loadedSource", _{ reason : Reason,
                                            source : _{ name : BaseName,
                                                        path : SourcePath
                                                      }
                                          }
             ),
    succ(Seq0, Seq).
da_server_handle_debugee_message(DebugeeThreadId,
                                 stopped(Reason, Description, Text, BreakpointIds),
                                 Out, Seq0, Seq) :-
    !,
    retract(da_server_debugee_thread(DebugeeThreadId, _)),
    asserta(da_server_debugee_thread(DebugeeThreadId, stopped)),
    dap_event(Out, Seq0, "stopped", _{ threadId         : DebugeeThreadId,
                                       reason           : Reason,
                                       description      : Description,
                                       text             : Text,
                                       hitBreakpointIds : BreakpointIds
                                     }
             ),
    succ(Seq0, Seq),
    (   da_server_disconnecting
    ->  safe_thread_send_message(DebugeeThreadId, disconnect)
    ;   true
    ).
da_server_handle_debugee_message(DebugeeThreadId,
                                 continued,
                                 Out, Seq0, Seq) :-
    !,
    retract(da_server_debugee_thread(DebugeeThreadId, _)),
    asserta(da_server_debugee_thread(DebugeeThreadId, running)),
    dap_event(Out, Seq0, "continued", _{ threadId            : DebugeeThreadId,
                                         allThreadsContinued : null
                                       }),
    succ(Seq0, Seq).
da_server_handle_debugee_message(_DebugeeThreadId,
                                 output(_Term, silent, _Lines),
                                 _Out, Seq, Seq) :- !.
da_server_handle_debugee_message(_DebugeeThreadId,
                                 output(Term, _Kind, _Lines),
                                 Out, Seq0, Seq) :-
    !,
    catch((   message_to_string(Term, String0),
              string_concat(String0, "\n", String),
              dap_event(Out, Seq0, "output", _{ category : "stdout",
                                                output   : String
                                              }),
              succ(Seq0, Seq)
          ),
          _Catcher,
          Seq = Seq0
         ).
da_server_handle_debugee_message(_DebugeeThreadId,
                                  stack_trace(RequestSeq, StackFrames0),
                                  Out, Seq0, Seq) :-
    !,
    maplist(prolog_dap_stack_frame, StackFrames0, StackFrames),
    dap_response(Out, Seq0, RequestSeq, "stackTrace", _{stackFrames:StackFrames}),
    succ(Seq0, Seq).
da_server_handle_debugee_message(_DebugeeThreadId,
                                  exception_info(RequestSeq, ExceptionTerm),
                                  Out, Seq0, Seq) :-
    !,
    prolog_dap_exception(ExceptionTerm, ExceptionId),
    dap_response(Out, Seq0, RequestSeq, "exceptionInfo", _{exceptionId:ExceptionId, description:ExceptionId}),
    succ(Seq0, Seq).
da_server_handle_debugee_message(_DebugeeThreadId,
                                  scopes(RequestSeq, Scopes0),
                                  Out, Seq0, Seq) :-
    !,
    maplist(prolog_dap_scope, Scopes0, Scopes),
    dap_response(Out, Seq0, RequestSeq, "scopes", _{scopes:Scopes}),
    succ(Seq0, Seq).
da_server_handle_debugee_message(_DebugeeThreadId,
                                  variables(RequestSeq, Variables0),
                                  Out, Seq0, Seq) :-
    !,
    maplist(prolog_dap_variable, Variables0, Variables),
    dap_response(Out, Seq0, RequestSeq, "variables", _{variables:Variables}),
    succ(Seq0, Seq).
da_server_handle_debugee_message(_DebugeeThreadId,
                                 step_in_targets(RequestSeq, Targets),
                                 Out, Seq0, Seq) :-
    !,
    maplist(prolog_dap_step_in_target, Targets, DAPTargets),
    dap_response(Out, Seq0, RequestSeq, "stepInTargets", _{targets:DAPTargets}),
    succ(Seq0, Seq).
da_server_handle_debugee_message(_DebugeeThreadId,
                                  evaluate(RequestSeq, Result, Bindings),
                                  Out, Seq0, Seq) :-
    !,
    format(string(Res), "~w~n~w.", [Bindings, Result]),
    dap_response(Out, Seq0, RequestSeq, "evaluate", _{result:Res, variablesReference:0}),
    succ(Seq0, Seq).
da_server_handle_debugee_message(_DebugeeThreadId,
                                  exited(_ExitCode),
                                  _Out, Seq, Seq) :- !.
da_server_handle_debugee_message(DebugeeThreadId,
                                 thread_exited,
                                 Out, Seq0, Seq) :-
    !,
    dap_event(Out, Seq0, "thread", _{ reason   : "exited",
                                      threadId : DebugeeThreadId
                                    }),
    succ(Seq0, Seq),
    retract(da_server_debugee_thread(DebugeeThreadId, _)),
    (   da_server_disconnecting, \+ da_server_debugee_thread(_, _)
    ->  dap_event(Out, Seq, "exited", _{exitCode:0}),
        thread_exit(0)
    ;   true
    ).
da_server_handle_debugee_message(DebugeeThreadId,
                                 thread_started,
                                 Out, Seq0, Seq) :-
    !,
    (   da_server_configured
    ->  thread_send_message(DebugeeThreadId, configuration_done)
    ;   true
    ),
    dap_event(Out, Seq0, "thread", _{ reason   : "started",
                                      threadId : DebugeeThreadId
                                    }),
    succ(Seq0, Seq),
    asserta(da_server_debugee_thread(DebugeeThreadId, running)),
    (   da_server_disconnecting
    ->  safe_thread_send_message(DebugeeThreadId, disconnect)
    ;   true
    ).

prolog_dap_step_in_target(step_in_target(Id, null), _{ id    : Id,
                                                       label : "step"
                                                     }) :- !.
prolog_dap_step_in_target(step_in_target(Id, _Alt), _{ id    : Id,
                                                       label : "fail"
                                                     }).

safe_thread_send_message(ThreadId, disconnect) :-
    catch(thread_send_message(ThreadId, disconnect), Catcher, true),
    expected_error(ThreadId, Catcher).


expected_error(ThreadId, error(existence_error(thread, ThreadId), _)) :- !.
expected_error(ThreadId, error(type_error(thread, ThreadId), _)).


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

prolog_dap_breakpoint(breakpoint(Id, Verified, Message, SourceSpan),
                 _{ id                 : Id,
                    verified           : Verified,
                    message            : Message,
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

prolog_dap_source_span(span(path(File), SL, SC, EL, EC),
                       _{ name            : Name,
                          path            : File,
                          origin          : "Static"
                        },
                       SL, SC, EL, EC
                      ) :-
    !,
    file_base_name(File, Name).
prolog_dap_source_span(span(reference(SourceReference), SL, SC, EL, EC),
                       _{ name            : "*dynamic*",
                          sourceReference : SourceReference,
                          origin          : "Dynamic"
                        },
                       SL, SC, EL, EC
                      ).

prolog_dap_stack_frame(stack_frame(Id, InFrameLabel, PI, _Alternative, SourceSpan),
                       _{ id                          : Id,
                          name                        : Name,
                          line                        : SL,
                          column                      : SC,
                          endLine                     : EL,
                          endColumn                   : EC,
                          source                      : DAPSource,
%                         alternative                 : DAPAlternative % for experiment with a custom protocol extension
                          instructionPointerReference : DAPLabel
                        }
                      ) :-
%   prolog_dap_alternative(Alternative, DAPAlternative),
    term_string(PI, Name),
    prolog_dap_source_span(SourceSpan, DAPSource, SL, SC, EL, EC),
    prolog_dap_in_frame_label(InFrameLabel, DAPLabel).

prolog_dap_exception(E, S) :- term_string(E, S).

prolog_dap_in_frame_label(port(Port), DAPLabel) :-
    !,
    functor(Port, PortName, _Arity),
    atom_string(PortName, DAPLabel).
prolog_dap_in_frame_label(pc(PC), DAPLabel) :-
    number_string(PC, DAPLabel).


:- dynamic da_server_configured/0.

:- det(da_server_command/6).
da_server_command("initialize", RequestSeq, Message, Out, Seq0, Seq) :-
    !,
    _{ arguments:Args } :< Message,
    da_server_capabilities(Capabilities),
    dap_response(Out, Seq0, RequestSeq, "initialize", Capabilities),
    succ(Seq0, Seq1),
    da_initialized(Args),
    dap_event(Out, Seq1, "initialized"),
    succ(Seq1, Seq).
da_server_command("launch", RequestSeq, Message, Out, Seq0, Seq) :-
    !,
    _{ arguments:Args } :< Message,
    da_launch(Args, Out, Seq0, Seq1),
    dap_response(Out, Seq1, RequestSeq, "launch"),
    succ(Seq1, Seq).
da_server_command("attach", RequestSeq, Message, Out, Seq0, Seq) :-
    !,
    _{ arguments:Args } :< Message,
    da_attach(Args, Out, Seq0, Seq1),
    dap_response(Out, Seq1, RequestSeq, "attach"),
    succ(Seq1, Seq).
da_server_command("configurationDone", RequestSeq, _Message, Out, Seq0, Seq) :-
    !,
    forall(da_server_debugee_thread(ThreadId, _), thread_send_message(ThreadId, configuration_done)),
    asserta(da_server_configured),
    dap_response(Out, Seq0, RequestSeq, "configurationDone"),
    succ(Seq0, Seq).
da_server_command("threads", RequestSeq, _Message, Out, Seq0, Seq) :-
    !,
    findall(_{ name : Name,
               id   : Id
            },
            (  da_server_debugee_thread(Id, _),
               term_string(Id, Name)
            ),
            Threads),
    dap_response(Out, Seq0, RequestSeq, "threads", _{threads:Threads}),
    succ(Seq0, Seq).
da_server_command("stackTrace", RequestSeq, Message, Out, Seq0, Seq) :-
    !,
    _{ arguments:Args } :< Message,
    _{ threadId:ThreadId } :< Args,
    catch((thread_send_message(ThreadId, stack_trace(RequestSeq)), Seq = Seq0),
          _Catcher,
          (dap_error(Out, Seq0, RequestSeq, "stackTrace", null), succ(Seq0, Seq))
         ).
da_server_command("exceptionInfo", RequestSeq, Message, Out, Seq0, Seq) :-
    !,
    _{ arguments:Args } :< Message,
    _{ threadId:ThreadId } :< Args,
    catch((thread_send_message(ThreadId, exception_info(RequestSeq)), Seq = Seq0),
          _Catcher,
          (dap_error(Out, Seq0, RequestSeq, "exceptionInfo", null), succ(Seq0, Seq))
         ).
da_server_command("pause", RequestSeq, Message, Out, Seq0, Seq) :-
    !,
    _{ arguments:Args } :< Message,
    _{ threadId:ThreadId } :< Args,
    (   da_server_debugee_thread(ThreadId, running)
    ->  dap_response(Out, Seq0, RequestSeq, "pause"),
        thread_signal(ThreadId, (retractall(da_tracer:da_tracer_last_action(_)),
                                 asserta(da_tracer:da_tracer_last_action(pause)))),
        thread_signal(ThreadId, trace)
    ;   dap_error(Out, Seq0, RequestSeq, "pause", "Thread is not running")
    ),
    succ(Seq0, Seq).
da_server_command("stepInTargets", RequestSeq, Message, _Out, Seq, Seq) :-
    !,
    _{ arguments : Args    } :< Message,
    _{ frameId   : FrameId } :< Args,
     forall(da_server_debugee_thread(ThreadId, _), thread_send_message(ThreadId, step_in_targets(RequestSeq, FrameId))).
da_server_command("stepIn", RequestSeq, Message, Out, Seq0, Seq) :-
    !,
    _{ arguments : Args     } :< Message,
    _{ threadId  : ThreadId } :< Args,
    (   get_dict(targetId, Args, Target)
    ->  true
    ;   Target = 0
    ),
    dap_response(Out, Seq0, RequestSeq, "stepIn"),
    succ(Seq0, Seq),
    thread_send_message(ThreadId, step_in(Target)).
da_server_command("stepOut", RequestSeq, Message, Out, Seq0, Seq) :-
    !,
    _{ arguments:Args } :< Message,
    _{ threadId:ThreadId } :< Args,
    dap_response(Out, Seq0, RequestSeq, "stepOut"),
    succ(Seq0, Seq),
    thread_send_message(ThreadId, step_out).
da_server_command("next", RequestSeq, Message, Out, Seq0, Seq) :-
    !,
    _{ arguments:Args } :< Message,
    _{ threadId:ThreadId } :< Args,
    dap_response(Out, Seq0, RequestSeq, "next"),
    succ(Seq0, Seq),
    thread_send_message(ThreadId, next).
da_server_command("continue", RequestSeq, Message, Out, Seq0, Seq) :-
    !,
    _{ arguments:Args } :< Message,
    _{ threadId:ThreadId } :< Args,
    dap_response(Out, Seq0, RequestSeq, "continue", _{allThreadsContinued:null}),
    succ(Seq0, Seq),
    thread_send_message(ThreadId, continue).
da_server_command("disconnect", RequestSeq, _Message, Out, Seq0, Seq) :-
    !,
    retractall(da_server_disconnecting),
    asserta(da_server_disconnecting),
    forall(da_server_debugee_thread(ThreadId, _), thread_send_message(ThreadId, disconnect)),
    dap_response(Out, Seq0, RequestSeq, "disconnect"),
    succ(Seq0, Seq),
    (   \+ da_server_debugee_thread(_, _)
    ->  dap_event(Out, Seq, "exited", _{exitCode:0}),
        thread_exit(0)
    ;   true
    ).
da_server_command("restartFrame", RequestSeq, Message, Out, Seq0, Seq) :-
    !,
    _{ arguments : Args    } :< Message,
    _{ frameId   : FrameId } :< Args,
    dap_response(Out, Seq0, RequestSeq, "restartFrame"),
    succ(Seq0, Seq),
    forall(da_server_debugee_thread(ThreadId, _), thread_send_message(ThreadId, restartFrame(FrameId))).
da_server_command("scopes", RequestSeq, Message, _Out, Seq, Seq) :-
    !,
    _{ arguments:Args } :< Message,
    _{ frameId:FrameId } :< Args,
    forall(da_server_debugee_thread(ThreadId, _), thread_send_message(ThreadId, scopes(RequestSeq, FrameId))).
da_server_command("variables", RequestSeq, Message, _Out, Seq, Seq) :-
    !,
    _{ arguments:Args } :< Message,
    _{ variablesReference:VariablesRef } :< Args,
    forall(da_server_debugee_thread(ThreadId, _), thread_send_message(ThreadId, variables(RequestSeq, VariablesRef))).
da_server_command("evaluate", RequestSeq, Message, _Out, Seq, Seq) :-
    !,
    _{ arguments  : Args } :< Message,
    _{ expression : SourceTerm,
       frameId    : FrameId,
       context    : _Context
     } :< Args,
    forall(da_server_debugee_thread(ThreadId, _), thread_send_message(ThreadId, evaluate(RequestSeq, FrameId, SourceTerm))).
da_server_command("setBreakpoints", RequestSeq, Message, Out, Seq0, Seq) :-
    !,
    _{ arguments : Args } :< Message,
    _{ source      : DAPSource,
       breakpoints : DAPReqBreakpoints
     } :< Args,
    dap_source_path(DAPSource, Path),
    maplist(dap_prolog_source_breakpoint(Path), DAPReqBreakpoints, ReqBreakpoints),
    da_breakpoints_set(Path, ReqBreakpoints, ResBreakpoints),
    maplist(prolog_dap_breakpoint, ResBreakpoints, DAPBreakpoints),
    dap_response(Out, Seq0, RequestSeq, "setBreakpoints", _{breakpoints:DAPBreakpoints}),
    succ(Seq0, Seq).
da_server_command("setExceptionBreakpoints", RequestSeq, Message, Out, Seq0, Seq) :-
    !,
    _{ arguments : Args    } :< Message,
    _{ filters   : Filters } :< Args,
    (   Filters == []
    ->  retractall(da_tracer:da_tracer_trapping),
        dap_response(Out, Seq0, RequestSeq, "setExceptionBreakpoints", _{breakpoints:[]})
    ;   Filters = ["true"|_]
    ->  asserta(da_tracer:da_tracer_trapping),
        dap_response(Out, Seq0, RequestSeq, "setExceptionBreakpoints", _{breakpoints:[_{verified:true}]})
    ),
    succ(Seq0, Seq).
da_server_command("setFunctionBreakpoints", RequestSeq, Message, Out, Seq0, Seq) :-
    !,
    _{ arguments   : Args              } :< Message,
    _{ breakpoints : DAPReqBreakpoints } :< Args,
    maplist(dap_prolog_function_breakpoint, DAPReqBreakpoints, ReqBreakpoints),
    nospyall,
    retractall(da_breakpoint:da_known_function_breakpoint(_)),
    findall(_{verified:Verified},
            (member(Spec, ReqBreakpoints),
             (   current_predicate(Spec)
             ->  asserta(da_breakpoint:da_known_function_breakpoint(Spec)),
                 spy(Spec), nodebug,
                 Verified = true
             ;   Verified = false
             )),
             DAPBreakpoints),
    dap_response(Out, Seq0, RequestSeq, "setFunctionBreakpoints", _{breakpoints:DAPBreakpoints}),
    succ(Seq0, Seq).
da_server_command("source", RequestSeq, Message, Out, Seq0, Seq) :-
    !,
    _{ arguments : Args } :< Message,
    _{ sourceReference : SourceReference
     } :< Args,
    (   integer(SourceReference), SourceReference > 0
    ->  da_source_clause_cached_reference(ClauseRef, SourceReference),
        da_clause_decompiled(ClauseRef, Module, DecompiledClause, VariablesOffset),
        da_clause_source_term(ClauseRef, Module, DecompiledClause, VariablesOffset, SourceClause, _, _),
        with_output_to(string(Content), portray_clause(current_output, SourceClause, [module(Module)])),
        dap_response(Out, Seq0, RequestSeq, "source", _{content:Content})
    ;   dap_error(Out, Seq0, RequestSeq, "source", "Cannot provide source code for requested predicate")
    ),
    succ(Seq0, Seq).
da_server_command(Command, RequestSeq, _Message, Out, Seq0, Seq) :-
    format(string(ErrorMessage), "Command \"~w\" is not implemented", [Command]),
    dap_error(Out, Seq0, RequestSeq, Command, ErrorMessage),
    succ(Seq0, Seq).


dap_source_path(D, path(P)     ) :- _{ path            : P0 } :< D, !, absolute_file_name(P0, P).
dap_source_path(D, reference(R)) :- _{ sourceReference : R  } :< D.

dap_prolog_function_breakpoint(D, user:Spec) :-
    get_dict(name, D, Name),
    term_string(Spec, Name).

dap_prolog_source_breakpoint(P, D, source_breakpoint(L, C, Cond, Hit, Log)) :-
    (   get_dict(line, D, L)
    ->  true
    ;   L = 0
    ),
    (   get_dict(column, D, C0)
    ->  true
    ;   C0 = 5    % 5 is a "guess" of the indentation.
    ),
    da_source_file_offsets_line_column_pairs(P, [C], [L-C0]),
    (   get_dict(condition, D, Cond)
    ->  true
    ;   Cond = "true"
    ),
    (   get_dict(logMessage, D, Log0)
    ->  Log = log_message(Log0)
    ;   Log = null
    ),
    (   get_dict(hitCondition, D, Hit0)
    ->  (   number(Hit0)
        ->  Hit = Hit0
        ;   number_string(Hit, Hit0)
        )
    ;   Hit = 0
    ).


da_server_capabilities(_{ supportsConfigurationDoneRequest  : true,
                          supportsExceptionInfoRequest      : true,
                          supportsRestartFrame              : true,
                          supportsEvaluateForHovers         : true,
                          supportsFunctionBreakpoints       : true,
                          supportsConditionalBreakpoints    : true,
                          supportsHitConditionalBreakpoints : true,
                          supportsLogPoints                 : true,
                          supportsStepInTargetsRequest      : true,
                          exceptionBreakpointFilters        : [ _{ filter : "true" , label : "Trap exceptions", default: false } ]
                        }
                      ).


da_initialized(_).


:- det(da_launch/4).
da_launch(Args, Out, Seq0, Seq) :-
    _{ goal: "$run_in_terminal" } :< Args,
    !,
    thread_self(ServerThreadId),
    tcp_socket(ServerSocket),
    tcp_setopt(ServerSocket, reuseaddr),
    tcp_bind(ServerSocket, Port),
    thread_create(da_terminal(ServerSocket, ServerThreadId), _PrologThreadId),
    number_string(Port, PortString),
    getenv('HOME', H),
    dap_request(Out, Seq0,
                "runInTerminal",
                _{   kind  : "integrated",
                     cwd   : H,
                     title : "Toplevel",
                     args  : ["telnet",  "127.0.0.1", PortString]
                 }),
    succ(Seq0, Seq).
da_launch(Args, _Out, Seq, Seq) :-
    _{ cwd: CWD, module: ModulePath, goal: GoalString } :< Args,
    !,
    cd(CWD),
    user:ensure_loaded(ModulePath),
    thread_self(ServerThreadId),
    thread_create(da_debugee(ModulePath, GoalString, ServerThreadId), _PrologThreadId).


da_attach(_Args, _Out, Seq, Seq).
