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
:- use_module(source).
:- use_module(clause).
:- use_module(breakpoint).

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
    debug(dap(server), "Received message ~w from stdin", [Message]),
    da_server_handled_message(Type, ClientSeq, Message, Out, W, State0, State, Seq0, Seq),
    debug(dap(server), "Handled message ~w from stdin", [Message]).
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
        debug(dap(server), "Handled message ~w from debugee thread ~w", [Message, DebugeeThreadId]),
        da_server_handled_debugee_messages(Out, State1, State, Seq1, Seq)
    ;   State = State0,
        Seq   = Seq0
    ).

:- det(da_server_handled_debugee_message/7).
da_server_handled_debugee_message(_DebugeeThreadId,
                                  loaded_source(Reason, SourcePath),
                                  Out, State, State, Seq0, Seq) :-
    !,
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
    !,
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
    !,
    maplist(prolog_dap_stack_frame, StackFrames0, StackFrames),
    dap_response(Out, Seq0, RequestSeq, "stackTrace", _{stackFrames:StackFrames}),
    succ(Seq0, Seq).
da_server_handled_debugee_message(_DebugeeThreadId,
                                  exception_info(RequestSeq, ExceptionTerm),
                                  Out, State, State, Seq0, Seq) :-
    !,
    prolog_dap_exception(ExceptionTerm, ExceptionId),
    dap_response(Out, Seq0, RequestSeq, "exceptionInfo", _{exceptionId:ExceptionId, description:ExceptionId}),
    succ(Seq0, Seq).
da_server_handled_debugee_message(_DebugeeThreadId,
                                  scopes(RequestSeq, Scopes0),
                                  Out, State, State, Seq0, Seq) :-
    !,
    maplist(prolog_dap_scope, Scopes0, Scopes),
    dap_response(Out, Seq0, RequestSeq, "scopes", _{scopes:Scopes}),
    succ(Seq0, Seq).
da_server_handled_debugee_message(_DebugeeThreadId,
                                  variables(RequestSeq, Variables0),
                                  Out, State, State, Seq0, Seq) :-
    !,
    maplist(prolog_dap_variable, Variables0, Variables),
    dap_response(Out, Seq0, RequestSeq, "variables", _{variables:Variables}),
    succ(Seq0, Seq).
da_server_handled_debugee_message(_DebugeeThreadId,
                                  evaluate(RequestSeq, Result, Bindings),
                                  Out, State, State, Seq0, Seq) :-
    !,
    format(string(Res), "~w~n~w.", [Bindings, Result]),
    dap_response(Out, Seq0, RequestSeq, "evaluate", _{result:Res, variablesReference:0}),
    succ(Seq0, Seq).
da_server_handled_debugee_message(_DebugeeThreadId,
                                  exited(_ExitCode),
                                  _Out, State, State, Seq, Seq) :- !.
%    dap_event(Out, Seq0, "exited", _{exitCode:ExitCode}),
%    succ(Seq0, Seq).
da_server_handled_debugee_message(DebugeeThreadId,
                                  thread_exited,
                                  Out, State0, State, Seq0, Seq) :-
    !,
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


da_server_handled_message("request", RequestSeq, Message0, Out, W, State0, State, Seq0, Seq) :-
    del_dict(command, Message0, Command, Message),
    da_server_command(Command, RequestSeq, Message, Out, W, State0, State, Seq0, Seq).
da_server_handled_message("response", _Seq, _Message, _Out, _W, State, State, Seq, Seq).

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
    da_launched(Args, Out, W, Debugee, Seq0, Seq1),
    dap_response(Out, Seq1, RequestSeq, "launch"),
    succ(Seq1, Seq).
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
da_server_command("exceptionInfo", RequestSeq, Message, Out, _W, State, State, Seq0, Seq) :-
    _{ arguments:Args } :< Message,
    _{ threadId:ThreadId } :< Args,
    catch((thread_send_message(ThreadId, exception_info(RequestSeq)), Seq = Seq0),
          _Catcher,
          (dap_error(Out, Seq0, RequestSeq, "exceptionInfo", null), succ(Seq0, Seq))
         ).
da_server_command("stepIn", RequestSeq, Message, Out, _W, State, State, Seq0, Seq) :-
    _{ arguments:Args } :< Message,
    _{ threadId:ThreadId } :< Args,
    dap_response(Out, Seq0, RequestSeq, "stepIn"),
    succ(Seq0, Seq),
    thread_send_message(ThreadId, step_in).
da_server_command("stepOut", RequestSeq, Message, Out, _W, State, State, Seq0, Seq) :-
    _{ arguments:Args } :< Message,
    _{ threadId:ThreadId } :< Args,
    dap_response(Out, Seq0, RequestSeq, "stepOut"),
    succ(Seq0, Seq),
    thread_send_message(ThreadId, step_out).
da_server_command("next", RequestSeq, Message, Out, _W, State, State, Seq0, Seq) :-
    _{ arguments:Args } :< Message,
    _{ threadId:ThreadId } :< Args,
    dap_response(Out, Seq0, RequestSeq, "next"),
    succ(Seq0, Seq),
    thread_send_message(ThreadId, next).
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
    maplist({FrameId}/[debugee(PrologThreadId, _, _)]>>thread_send_message(PrologThreadId, restart_frame(FrameId)), State).
da_server_command("scopes", RequestSeq, Message, _Out, _W, State, State, Seq, Seq) :-
    _{ arguments:Args } :< Message,
    _{ frameId:FrameId } :< Args,
    maplist({RequestSeq, FrameId}/[debugee(PrologThreadId, _, _)]>>thread_send_message(PrologThreadId, scopes(RequestSeq, FrameId)), State).
da_server_command("variables", RequestSeq, Message, _Out, _W, State, State, Seq, Seq) :-
    _{ arguments:Args } :< Message,
    _{ variablesReference:VariablesRef } :< Args,
    maplist({RequestSeq, VariablesRef}/[debugee(PrologThreadId, _, _)]>>thread_send_message(PrologThreadId, variables(RequestSeq, VariablesRef)), State).
da_server_command("evaluate", RequestSeq, Message, _Out, _W, State, State, Seq, Seq) :-
    _{ arguments  : Args } :< Message,
    _{ expression : SourceTerm,
       frameId    : FrameId,
       context    : Context
     } :< Args,
    debug(dap(tracer), "handling evaluate request arguments with ~w ~w ~w", [FrameId, SourceTerm, Context]),
    maplist({RequestSeq, FrameId, SourceTerm}/[debugee(PrologThreadId, _, _)]>>thread_send_message(PrologThreadId, evaluate(RequestSeq, FrameId, SourceTerm)), State).
da_server_command("setBreakpoints", RequestSeq, Message, Out, _W, State, State, Seq0, Seq) :-
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
da_server_command("source", RequestSeq, Message, Out, _W, State, State, Seq0, Seq) :-
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
da_server_command(Command, RequestSeq, _Message, Out, _W, State, State, Seq0, Seq) :-
    format(string(ErrorMessage), "Command \"~w\" is not implemented", [Command]),
    dap_error(Out, Seq0, RequestSeq, Command, ErrorMessage),
    succ(Seq0, Seq).

dap_source_path(D, path(P)) :- _{ path : P0 } :< D, !, atom_string(P, P0).
dap_source_path(D, reference(R)) :- _{ sourceReference : R } :< D.

dap_prolog_source_breakpoint(P, D, source_breakpoint(L, C)) :-
    _{ line   : L,
       column : C0
     } :< D, !,
    da_source_file_offsets_line_column_pairs(P, [C], [L-C0]).
dap_prolog_source_breakpoint(P, D, source_breakpoint(L, C)) :-
    _{ line   : L } :< D,
    da_source_file_offsets_line_column_pairs(P, [C], [L-5]).  % 5 is a "guess" of the indentation. TODO - locate first term in line intelligently


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

da_server_capabilities(_{ supportsConfigurationDoneRequest : true,
                          supportsExceptionInfoRequest     : true,
                          supportsRestartFrame             : true,
                          supportsEvaluateForHovers        : true
                        }
                      ).

da_initialized(_).

:- det(da_launched/6).
da_launched(Args, Out, W, debugee(PrologThreadId, ThreadId, run_in_terminal), Seq0, Seq) :-
    _{ goal: "$run_in_terminal" } :< Args,
    !,
    thread_self(ServerThreadId),
    tcp_socket(ServerSocket),
    tcp_setopt(ServerSocket, reuseaddr),
    tcp_bind(ServerSocket, Port),
    thread_create(da_terminal(ServerSocket, ServerThreadId, W), PrologThreadId),
    number_string(Port, PortString),
    getenv('HOME', H),
    dap_request(Out, Seq0,
                "runInTerminal",
                _{   kind  : "integrated",
                     cwd   : H,
                     title : "Toplevel",
                     args  : ["telnet",  "127.0.0.1", PortString]
                 }),
    succ(Seq0, Seq),
    thread_property(PrologThreadId, id(ThreadId)).

da_launched(Args, _Out, W, debugee(PrologThreadId, ThreadId, Goal), Seq, Seq) :-
    _{ cwd: CWD, module: ModulePath, goal: GoalString } :< Args,
    !,
    cd(CWD),
    term_string(Goal, GoalString),
    thread_self(ServerThreadId),
    thread_create(da_debugee(ModulePath, Goal, ServerThreadId, W), PrologThreadId),
    thread_property(PrologThreadId, id(ThreadId)).

da_configured([debugee(_, ThreadId, _)|T]) :-
    thread_send_message(ThreadId, configuration_done),
    da_configured(T).
da_configured([]).
