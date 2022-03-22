:- module(
       swipl_debug_adapter,
       [
           swipl_debug_adapter_command_callback/6
       ]
   ).

:- use_module(library(debug_adapter/compat)).
:- use_module(library(debug_adapter/sdk)).
:- use_module(library(swipl_debug_adapter/stack)).
:- use_module(library(swipl_debug_adapter/source)).
:- use_module(library(swipl_debug_adapter/frame)).
:- use_module(library(swipl_debug_adapter/clause)).


swipl_debug_adapter_command_callback(disconnect, _Arguments, ReqSeq, Handle, [], disconnected) :-
    !,
    debug(dap(swipl), "Disconnecting", []),
    da_sdk_response(Handle, ReqSeq, disconnect),
    da_sdk_stop(Handle).
swipl_debug_adapter_command_callback(initialize, Arguments, ReqSeq, Handle, [], initialized(Arguments)) :-
    !,
    debug(dap(swipl), "Initializing", []),
    swipl_debug_adapter_capabilities(Capabilities),
    da_sdk_response(Handle, ReqSeq, initialize, Capabilities),
    da_sdk_event(Handle, initialized).
swipl_debug_adapter_command_callback(launch, Arguments, ReqSeq, Handle, initialized(_), configured([Thread])) :-
    !,
    debug(dap(swipl), "Launching ~w", [Arguments]),
    swipl_debug_adapter_launch_thread(Arguments, Handle, Thread),
    da_sdk_response(Handle, ReqSeq, launch).
swipl_debug_adapter_command_callback(disconnect, _Arguments, ReqSeq, Handle, initialized(_), disconnected) :-
    !,
    debug(dap(swipl), "Disconnecting", []),
    da_sdk_response(Handle, ReqSeq, disconnect),
    da_sdk_event(Handle, exited, _{ exitCode : 0 }),
    da_sdk_stop(Handle).
swipl_debug_adapter_command_callback(configurationDone, _Arguments, ReqSeq, Handle, initialized(_), configured([])) :-
    !,
    debug(dap(swipl), "Finalizing configuration", []),
    da_sdk_response(Handle, ReqSeq, configurationDone).
swipl_debug_adapter_command_callback(launch, Arguments, ReqSeq, Handle, configured(Threads), configured([Thread|Threads])) :-
    !,
    debug(dap(swipl), "Launching ~w", [Arguments]),
    swipl_debug_adapter_launch_thread(Arguments, Handle, Thread),
    da_sdk_response(Handle, ReqSeq, launch).
swipl_debug_adapter_command_callback(configurationDone, _Arguments, ReqSeq, Handle, configured(Threads), configured(Threads)) :-
    !,
    da_sdk_response(Handle, ReqSeq, configurationDone).
swipl_debug_adapter_command_callback(threads, _Arguments, ReqSeq, Handle, configured(Threads), configured(Threads)) :-
    !,
    debug(dap(swipl), "Handling threads request", []),
    maplist(number_string, Threads, Names),
    maplist([I,N,_{id:I,name:N}]>>true, Threads, Names, Ts),
    da_sdk_response(Handle, ReqSeq, threads, _{threads:Ts}).
swipl_debug_adapter_command_callback(pause, Arguments, ReqSeq, Handle, configured(Threads0), configured(Threads)) :-
    !,
    debug(dap(swipl), "Handling pause request", []),
    _{ threadId : ThreadId } :< Arguments,
    select(ThreadId, Threads0, Threads1),
    catch((thread_signal(ThreadId, (retractall(swipl_debug_adapter_last_action(_)),
                                    asserta(swipl_debug_adapter_last_action(pause)))),
           thread_signal(ThreadId, trace),
           da_sdk_response(Handle, ReqSeq, pause),
           Threads = Threads0),
          _,
          (da_sdk_error(Handle, ReqSeq, pause, "Cannot pause requested thread"),
           Threads = Threads1)).
swipl_debug_adapter_command_callback(source, Arguments, ReqSeq, Handle, State, State) :-
    !,
    _{ sourceReference : SourceReference } :< Arguments,
    (   integer(SourceReference), SourceReference > 0
    ->  da_source_clause_cached_reference(ClauseRef, SourceReference),
        da_clause_decompiled(ClauseRef, Module, DecompiledClause, VariablesOffset),
        da_clause_source_term(ClauseRef, Module, DecompiledClause, VariablesOffset, SourceClause, _, _),
        with_output_to(string(Content), portray_clause(current_output, SourceClause, [module(Module)])),
        da_sdk_response(Handle, ReqSeq, source, _{content:Content})
    ;   da_sdk_error(Handle, ReqSeq, source, "Cannot provide source code for requested predicate")
    ).
swipl_debug_adapter_command_callback(exceptionInfo, Arguments, ReqSeq, _Handle, configured(Threads0), configured(Threads)) :-
    !,
    debug(dap(swipl), "Handling exceptionInfo request", []),
    _{ threadId : ThreadId } :< Arguments,
    select(ThreadId, Threads0, Threads1),
    catch((thread_send_message(ThreadId, exception_info(ReqSeq)), Threads = Threads0),
          _,
          Threads = Threads1).
swipl_debug_adapter_command_callback(stackTrace, Arguments, ReqSeq, _Handle, configured(Threads0), configured(Threads)) :-
    !,
    debug(dap(swipl), "Handling stackTrace request", []),
    _{ threadId : ThreadId } :< Arguments,
    select(ThreadId, Threads0, Threads1),
    catch((thread_send_message(ThreadId, stack_trace(ReqSeq)), Threads = Threads0),
          _,
          Threads = Threads1).
swipl_debug_adapter_command_callback(evaluate, Arguments, ReqSeq, _Handle, configured(Threads0), configured(Threads)) :-
    !,
    debug(dap(swipl), "Handling evaluate request", []),
    _{ frameId : FrameId, expression : Expression } :< Arguments,
    include({ReqSeq, FrameId}/[T]>>catch(thread_send_message(T, evaluate(ReqSeq, FrameId, Expression)),
                                         _,
                                         fail),
            Threads0,
            Threads).
swipl_debug_adapter_command_callback(scopes, Arguments, ReqSeq, _Handle, configured(Threads0), configured(Threads)) :-
    !,
    debug(dap(swipl), "Handling scopes request", []),
    _{ frameId : FrameId } :< Arguments,
    include({ReqSeq, FrameId}/[T]>>catch(thread_send_message(T, scopes(ReqSeq, FrameId)),
                                         _,
                                         fail),
            Threads0,
            Threads).
swipl_debug_adapter_command_callback(variables, Arguments, ReqSeq, _Handle, configured(Threads0), configured(Threads)) :-
    !,
    debug(dap(swipl), "Handling variables request", []),
    _{ variablesReference : VariablesRef } :< Arguments,
    include({ReqSeq, VariablesRef}/[T]>>catch(thread_send_message(T, variables(ReqSeq, VariablesRef)),
                                         _,
                                         fail),
            Threads0,
            Threads).
swipl_debug_adapter_command_callback(stepInTargets, Arguments, ReqSeq, _Handle, configured(Threads0), configured(Threads)) :-
    !,
    debug(dap(swipl), "Handling stepInTargets request", []),
    _{ frameId : FrameId } :< Arguments,
    include({ReqSeq, FrameId}/[T]>>catch(thread_send_message(T, step_in_targets(ReqSeq, FrameId)),
                                         _,
                                         fail),
            Threads0,
            Threads).
swipl_debug_adapter_command_callback(stepIn, Arguments, ReqSeq, _Handle, configured(Threads0), configured(Threads)) :-
    !,
    debug(dap(swipl), "Handling stepIn request", []),
    _{ threadId : ThreadId } :< Arguments,
    select(ThreadId, Threads0, Threads1),
    (   get_dict(targetId, Arguments, Target)
    ->  true
    ;   Target = 0
    ),
    catch((thread_send_message(ThreadId, step_in(ReqSeq, Target)), Threads = Threads0),
          _,
          Threads = Threads1).
swipl_debug_adapter_command_callback(next, Arguments, ReqSeq, _Handle, configured(Threads0), configured(Threads)) :-
    !,
    debug(dap(swipl), "Handling next request", []),
    _{ threadId : ThreadId } :< Arguments,
    select(ThreadId, Threads0, Threads1),
    catch((thread_send_message(ThreadId, next(ReqSeq)), Threads = Threads0),
          _,
          Threads = Threads1).
swipl_debug_adapter_command_callback(stepOut, Arguments, ReqSeq, _Handle, configured(Threads0), configured(Threads)) :-
    !,
    debug(dap(swipl), "Handling stepOut request", []),
    _{ threadId : ThreadId } :< Arguments,
    select(ThreadId, Threads0, Threads1),
    catch((thread_send_message(ThreadId, step_out(ReqSeq)), Threads = Threads0),
          _,
          Threads = Threads1).
swipl_debug_adapter_command_callback(restartFrame, Arguments, ReqSeq, _Handle, configured(Threads0), configured(Threads)) :-
    !,
    debug(dap(swipl), "Handling stepOut request", []),
    _{ frameId : FrameId } :< Arguments,
    include({ReqSeq, FrameId}/[T]>>catch(thread_send_message(T, restart_frame(ReqSeq, FrameId)),
                                         _,
                                         fail),
            Threads0,
            Threads).
swipl_debug_adapter_command_callback(setFunctionBreakpoints, Arguments, ReqSeq, Handle, State, State) :-
    !,
    debug(dap(swipl), "Handling setFunctionBreakpoints request", []),
    _{ breakpoints : ReqBreakpoints } :< Arguments,
    maplist(swipl_debug_adapter_translate_function_breakpoint, ReqBreakpoints, PIs),
    retractall(swipl_debug_adapter_function_breakpoint(_)),
    nospyall,
    findall(_{verified:Verified},
            (member(PI, PIs),
             (   pi_head(PI, Head),
                 predicate_property(Head, defined)
             ->  asserta(swipl_debug_adapter_function_breakpoint(PI)),
                 spy(PI), nodebug,
                 Verified = true
             ;   Verified = false)),
            ResBreakpoints),
    da_sdk_response(Handle, ReqSeq, setFunctionBreakpoints, _{breakpoints:ResBreakpoints}).
swipl_debug_adapter_command_callback(setExceptionBreakpoints, Arguments, ReqSeq, Handle, State, State) :-
    !,
    debug(dap(swipl), "Handling setExceptionBreakpoints request", []),
    _{ filters : Filters } :< Arguments,
    (   Filters == []
    ->  retractall(swipl_debug_adapter_trapping),
        da_sdk_response(Handle, ReqSeq, setExceptionBreakpoints, _{breakpoints:[]})
    ;   Filters = ["true"|_]
    ->  asserta(swipl_debug_adapter_trapping),
        da_sdk_response(Handle, ReqSeq, setExceptionBreakpoints, _{breakpoints:[_{verified:true}]})
    ).
swipl_debug_adapter_command_callback(setBreakpoints, Arguments, ReqSeq, Handle, State, State) :-
    !,
    debug(dap(swipl), "Handling setBreakpoints request", []),
    _{ source      : DAPSource,
       breakpoints : DAPReqBreakpoints
     } :< Arguments,
    dap_source_path(DAPSource, Path),
    maplist(swipl_debug_adapter_translate_source_breakpoint(Path), DAPReqBreakpoints, ReqBreakpoints),
    swipl_debug_adapter_breakpoints_set(Path, ReqBreakpoints, ResBreakpoints),
    maplist(swipl_debug_adapter_translate_result_breakpoint, ResBreakpoints, DAPBreakpoints),
    da_sdk_response(Handle, ReqSeq, setBreakpoints, _{breakpoints:DAPBreakpoints}).
swipl_debug_adapter_command_callback(disconnect, _Arguments, ReqSeq, Handle, configured(Threads), disconnected) :-
    !,
    debug(dap(swipl), "disconnecting", []),
    maplist([T]>>catch(thread_send_message(T, disconnect), _, true), Threads),
    da_sdk_response(Handle, ReqSeq, disconnect),
    da_sdk_stop(Handle).
swipl_debug_adapter_command_callback(continue, Arguments, ReqSeq, Handle, configured(Threads), configured(Threads)) :-
    !,
    debug(dap(swipl), "Handling continue request", []),
    _{ threadId : ThreadId } :< Arguments,
    select(ThreadId, Threads0, Threads1),
    catch((thread_send_message(ThreadId, continue), Threads = Threads0),
          _,
          Threads = Threads1),
    da_sdk_response(Handle, ReqSeq, continue).


swipl_debug_adapter_capabilities(_{ supportsConfigurationDoneRequest  : true,
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

dap_source_path(D, path(P)     ) :- _{ path            : P0 } :< D, !, absolute_file_name(P0, P).
dap_source_path(D, reference(R)) :- _{ sourceReference : R  } :< D.

swipl_debug_adapter_launch_thread(Args, Handle, ThreadId) :-
    _{ cwd: CWD, module: ModulePath, goal: GoalString } :< Args,
    !,
    cd(CWD),
    user:ensure_loaded(ModulePath),
    thread_self(ServerThreadId),
    thread_create(swipl_debug_adapter_debugee(ModulePath, GoalString, ServerThreadId, Handle), PrologThreadId),
    thread_get_message(started(PrologThreadId)),
    thread_property(PrologThreadId, id(ThreadId)).


:- det(swipl_debug_adapter_debugee/4).
swipl_debug_adapter_debugee(ModulePath, GoalString, ServerThreadId, Handle) :-
    thread_self(Self),
    thread_send_message(ServerThreadId, started(Self)),
    thread_property(Self, id(Id)),
    da_sdk_event(Handle, thread, _{ reason   : "started",
                                    threadId : Id }),
    term_string(Goal, GoalString, [variable_names(VarNames)]),
    absolute_file_name(ModulePath, AbsModulePath, []),
    user:ensure_loaded(AbsModulePath),
    (   module_property(Module, file(AbsModulePath))
    ->  qualified(QGoal, Module, Goal)
    ;   QGoal = Goal
    ),
    swipl_debug_adapter_trace(QGoal, VarNames, Handle).


swipl_debug_adapter_translate_exit_code(true        , 0) :- !.
swipl_debug_adapter_translate_exit_code(false       , 1) :- !.
swipl_debug_adapter_translate_exit_code(exception(_), 2) :- !.

swipl_debug_adapter_translate_source_breakpoint(P, D, source_breakpoint(L, C, Cond, Hit, Log)) :-
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


swipl_debug_adapter_translate_variable(variable(Name, Value, VariablesRef),
				       _{ name               : Name,
					  variablesReference : VariablesRef,
					  value              : Value }).


swipl_debug_adapter_translate_result_breakpoint(breakpoint(Id, Verified, Message, SourceSpan),
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
    swipl_debug_adapter_translate_source_span(SourceSpan, DAPSource, SL, SC, EL, EC).


swipl_debug_adapter_translate_source_span(span(path(File), SL, SC, EL, EC),
                                          _{ name            : Name,
                                             path            : File,
                                             origin          : "Static"
                                           },
                                          SL, SC, EL, EC
                                         ) :-
    !,
    file_base_name(File, Name).
swipl_debug_adapter_translate_source_span(span(reference(SourceReference), SL, SC, EL, EC),
                                          _{ name            : "*dynamic*",
                                             sourceReference : SourceReference,
                                             origin          : "Dynamic"
                                           },
                                          SL, SC, EL, EC
                                         ).


swipl_debug_adapter_translate_function_breakpoint(D, M:P) :-
    get_dict(name, D, S),
    term_string(M:P, S),
    !.
swipl_debug_adapter_translate_function_breakpoint(D, user:P) :-
    get_dict(name, D, S),
    term_string(P, S).

:- thread_local
   swipl_debug_adapter_last_action/1.

:- dynamic
   swipl_debug_adapter_handle/1,
   swipl_debug_adapter_trapping/0,
   swipl_debug_adapter_source_breakpoint/7,
   swipl_debug_adapter_function_breakpoint/1.


:- det(swipl_debug_adapter_trace/3).
swipl_debug_adapter_trace(QGoal, VarNames, Handle) :-
    asserta(swipl_debug_adapter_handle(Handle), Ref),
    asserta(swipl_debug_adapter_last_action(entry)),
    asserta((user:thread_message_hook(Term, Kind, Lines) :-
                 swipl_debug_adapter_message_hook(Term, Kind, Lines),
                 fail)),
    asserta((user:prolog_exception_hook(Ex, Out, Frame, Catcher) :-
                 swipl_debug_adapter_exception_hook(Ex, Out, Frame, Catcher),
                 fail)),
    prolog_listen(break, swipl_debug_adapter_handle_break_event, [as(last), name(swipl_debug_adapter)]),
    set_prolog_flag(gui_tracer, true),
    visible([+call, +exit, +fail, +redo, +unify, +cut_call, +cut_exit, +exception]),
    prolog_skip_level(_, very_deep),
    swipl_debug_adapter_goal_reified_result(QGoal, VarNames, Result),
    prolog_listen(break, swipl_debug_adapter_mock_break_event, [as(last), name(swipl_debug_adapter)]),
    erase(Ref),
    thread_self(Self),
    thread_property(Self, id(Id)),
    da_sdk_event(Handle, thread, _{ reason   : "exited",
                                    threadId : Id }),
    swipl_debug_adapter_translate_exit_code(Result, ExitCode),
    da_sdk_event(Handle, exited, _{ exitCode : ExitCode }).


swipl_debug_adapter_goal_reified_result(Goal, VarNames, Result) :-
    catch((   trace, Goal, notrace
          ->  print_message(trace, da_tracer_top_level_query(true(VarNames))),
              Result = true
          ;   notrace,
              print_message(trace, da_tracer_top_level_query(false)),
              Result = false
          ),
          Catcher,
          (notrace,
           print_message(trace, da_tracer_top_level_query(exception(Catcher))),
           Result = exception(Catcher)
          )
         ).


swipl_debug_adapter_handle_break_event(gc, ClauseRef, PC) :-
    debug(dap(swipl), "Handling breakpoint event", []),
    swipl_debug_adapter_handle(Handle),
    !,
    (   retract(swipl_debug_adapter_source_breakpoint(BP, ClauseRef, PC, _, _, _, _))
    ->  da_sdk_event(Handle, breakpoint, _{ reason     : "removed",
                                            breakpoint : _{ id       : BP,
                                                            verified : false }})
    ;   true
    ).


swipl_debug_adapter_mock_break_event(_, _, _) :- fail.


swipl_debug_adapter_exception_hook(_In, _Out, _Frame, _Catcher) :-
    thread_self(Me),
    thread_property(Me, debug(true)),
    swipl_debug_adapter_trapping,
    trace.


:- det(swipl_debug_adapter_message_hook/3).
swipl_debug_adapter_message_hook(_   , silent, _) :- !.
swipl_debug_adapter_message_hook(Term, _     , _) :-
    swipl_debug_adapter_handle(Handle),
    phrase(prolog:message(Term), Lines),
    print_message_lines(string(String), '', Lines),
    da_sdk_event(Handle, output, _{output:String, category:"stdout"}).


:- multifile prolog:message//1.

prolog:message(da_tracer_top_level_query(true([]))) -->
    !,
    [ 'true.'-[] ].
prolog:message(da_tracer_top_level_query(true(VarNames))) -->
    !,
    [ '~p'-[VarNames], nl ],
    [ 'true.'-[] ].
prolog:message(da_tracer_top_level_query(false)) -->
    !,
    [ 'false.'-[] ].
prolog:message(da_tracer_top_level_query(exception(E))) -->
    !,
    [ 'unhandled exception: ~w.'-[E] ].
prolog:message(log_message(BP, Map, String0)) -->
    { interpolate_string(String0, String, Map, []) },
    !,
    [ 'Log point ~w: ~w'-[BP, String] ].


user:prolog_trace_interception(Port, Frame, Choice, Action) :-
    notrace(swipl_debug_adapter_trace_interception(Port, Frame, Choice, Action)),
    swipl_debug_adapter_tracer_yield(Action).


swipl_debug_adapter_trace_interception(Port, Frame, Choice, Action) :-
    swipl_debug_adapter_handle(Handle),
    !,
    swipl_debug_adapter_last_action(LastAction),
    !,
    swipl_debug_adapter_stopped(Port, Frame, Choice, LastAction, Handle, Action).


:- det(swipl_debug_adapter_stopped/6).
swipl_debug_adapter_stopped(Port, Frame, Choice, LastAction, Handle, Action) :-
    swipl_debug_adapter_stopped_reason(Port, Frame, LastAction, Reason),
    thread_self(Self),
    thread_property(Self, id(Id)),
    put_dict(threadId, Reason, Id, Body),
    da_sdk_event(Handle, stopped, Body),
    swipl_debug_adapter_handle_messages(Port, Frame, Choice, Handle, Action).

swipl_debug_adapter_handle_messages(Port, Frame, Choice, Handle, Action) :-
    thread_get_message(M),
    swipl_debug_adapter_handle_message(M, Port, Frame, Choice, Handle, Action).

swipl_debug_adapter_handle_message(continue, _Port, _Frame, _Choice, Handle, continue) :-
    !,
    thread_self(Self),
    thread_property(Self, id(Id)),
    da_sdk_event(Handle, continued, _{threadId:Id}),
    retractall(swipl_debug_adapter_last_action(_)),
    asserta(swipl_debug_adapter_last_action(continue)).
swipl_debug_adapter_handle_message(stack_trace(ReqSeq), Port, Frame, Choice, Handle, Action) :-
    !,
    swipl_debug_adapter_stack_trace(Port, Frame, Choice, StackFrames),
    da_sdk_response(Handle, ReqSeq, stackTrace, _{stackFrames:StackFrames}),
    swipl_debug_adapter_handle_messages(Port, Frame, Choice, Handle, Action).
swipl_debug_adapter_handle_message(disconnect, _Port, _Frame, _Choice, Handle, nodebug) :-
    !,
    thread_self(Self),
    thread_property(Self, id(Id)),
    da_sdk_event(Handle, continued, _{threadId:Id}),
    retractall(swipl_debug_adapter_last_action(_)),
    asserta(swipl_debug_adapter_last_action(continue)).
swipl_debug_adapter_handle_message(exception_info(ReqSeq), Port, Frame, Choice, Handle, Action) :-
    !,
    (   Port = exception(Exception)
    ->  term_string(Exception, String),
        da_sdk_response(Handle, ReqSeq, exceptionInfo, _{exceptionId:String, description:String})
    ;   da_sdk_error(Handle, ReqSeq, exceptionInfo, "No exceptionInfo available")
    ),
    swipl_debug_adapter_handle_messages(Port, Frame, Choice, Handle, Action).
swipl_debug_adapter_handle_message(evaluate(ReqSeq, FrameId, SourceTerm), Port, Frame, Choice, Handle, Action) :-
    !,
    da_frame_evaluate(FrameId, SourceTerm, Result, Bindings),
    format(string(Res), "~w~n~w.", [Bindings, Result]),
    da_sdk_response(Handle, ReqSeq, evaluate, _{result:Res, variablesReference:0}),
    swipl_debug_adapter_handle_messages(Port, Frame, Choice, Handle, Action).
swipl_debug_adapter_handle_message(step_in_targets(ReqSeq, FrameId), Port, Frame, Choice, Handle, Action) :-
    !,
    da_frame_step_in_targets(FrameId, Frame, Choice, Targets),
    maplist(swipl_debug_adapter_translate_step_in_target, Targets, DAPTargets),
    da_sdk_response(Handle, ReqSeq, stepInTargets, _{targets:DAPTargets}),
    swipl_debug_adapter_handle_messages(Port, Frame, Choice, Handle, Action).
swipl_debug_adapter_handle_message(scopes(ReqSeq, FrameId), Port, Frame, Choice, Handle, Action) :-
    !,
    da_frame_scopes(FrameId, Frame, Port, Scopes),
    maplist(swipl_debug_adapter_translate_scope, Scopes, DAPScopes),
    da_sdk_response(Handle, ReqSeq, scopes, _{scopes:DAPScopes}),
    swipl_debug_adapter_handle_messages(Port, Frame, Choice, Handle, Action).
swipl_debug_adapter_handle_message(variables(ReqSeq, VariablesRef), Port, Frame, Choice, Handle, Action) :-
    !,
    da_referenced_variables(VariablesRef, Variables),
    maplist(swipl_debug_adapter_translate_variable, Variables, DAPVariables),
    da_sdk_response(Handle, ReqSeq, variables, _{variables:DAPVariables}),
    swipl_debug_adapter_handle_messages(Port, Frame, Choice, Handle, Action).
swipl_debug_adapter_handle_message(step_in(ReqSeq, 0), _Port, _Frame, _Choice, Handle, continue) :-
    !,
    da_sdk_response(Handle, ReqSeq, stepIn),
    retractall(swipl_debug_adapter_last_action(_)),
    asserta(swipl_debug_adapter_last_action(step_in)).
swipl_debug_adapter_handle_message(step_in(ReqSeq, 1), _Port, _Frame, _Choice, Handle, fail) :-
    !,
    da_sdk_response(Handle, ReqSeq, stepIn),
    retractall(swipl_debug_adapter_last_action(_)),
    asserta(swipl_debug_adapter_last_action(step_in)).
swipl_debug_adapter_handle_message(next(ReqSeq), _Port, _Frame, _Choice, Handle, skip) :-
    !,
    da_sdk_response(Handle, ReqSeq, next),
    retractall(swipl_debug_adapter_last_action(_)),
    asserta(swipl_debug_adapter_last_action(next)).
swipl_debug_adapter_handle_message(step_out(ReqSeq), _Port, _Frame, _Choice, Handle, up) :-
    !,
    da_sdk_response(Handle, ReqSeq, stepOut),
    retractall(swipl_debug_adapter_last_action(_)),
    asserta(swipl_debug_adapter_last_action(step_out)).
swipl_debug_adapter_handle_message(restart_frame(ReqSeq, FrameId), _Port, _Frame, _Choice, Handle, retry(FrameId)) :-
    !,
    da_sdk_response(Handle, ReqSeq, restartFrame),
    retractall(swipl_debug_adapter_last_action(_)),
    asserta(swipl_debug_adapter_last_action(restart_frame)).


swipl_debug_adapter_stopped_reason(exception(E), _, _            , _{reason:exception, description:D}) :- !, term_string(E, D).
swipl_debug_adapter_stopped_reason(call        , _, entry        , _{reason:entry}) :- !.
swipl_debug_adapter_stopped_reason(_           , _, step_in      , _{reason:step}) :- !.
swipl_debug_adapter_stopped_reason(_           , _, step_out     , _{reason:step}) :- !.
swipl_debug_adapter_stopped_reason(_           , _, next         , _{reason:step}) :- !.
swipl_debug_adapter_stopped_reason(_           , _, restart_frame, _{reason:restart}) :- !.
swipl_debug_adapter_stopped_reason(_           , _, pause        , _{reason:pause}) :- !.
swipl_debug_adapter_stopped_reason(_           , _, breakpoint(B), _{reason:breakpoint, hitBreakpointIds:[B]}) :- !.
swipl_debug_adapter_stopped_reason(call        , F, _            , _{reason:"function breakpoint"}) :-
    prolog_frame_attribute(F, predicate_indicator, PI),
    (   PI = _:_
    ->  swipl_debug_adapter_function_breakpoint(PI)
    ;   swipl_debug_adapter_function_breakpoint(user:PI)
    ),
    !.
swipl_debug_adapter_stopped_reason(_           , _, _            , _{reason:trace}).


swipl_debug_adapter_loop(Port, Frame, Choice, Action, Handle) :-
    thread_get_message(Message),
    swipl_debug_adapter_handle_message(Message, Port, Frame, Choice, Action0, Handle),
    (   Action0 == loop
    ->  swipl_debug_adapter_loop(Port, Frame, Choice, Action, Handle)
    ;   Action  = Action0
    ).


swipl_debug_adapter_tracer_yield(skip) :-
    trace.
swipl_debug_adapter_tracer_yield(retry) :-
    prolog_skip_level(_, very_deep),
    trace.
swipl_debug_adapter_tracer_yield(retry(_)) :-
    prolog_skip_level(_, very_deep),
    trace.
swipl_debug_adapter_tracer_yield(fail) :-
    prolog_skip_level(_, very_deep),
    trace.
swipl_debug_adapter_tracer_yield(continue) :-
    swipl_debug_adapter_last_action(continue),
    !,
    notrace,
    prolog_skip_level(_, very_deep),
    debug.
swipl_debug_adapter_tracer_yield(continue) :-
    prolog_skip_level(_, very_deep),
    trace.
swipl_debug_adapter_tracer_yield(up) :-
    prolog_skip_level(_, very_deep),
    trace.
swipl_debug_adapter_tracer_yield(nodebug).
swipl_debug_adapter_tracer_yield(abort).
swipl_debug_adapter_tracer_yield(ignore) :-
    trace.


swipl_debug_adapter_stack_trace(Port, Frame, Choice, StackTrace) :-
    da_stack_frame_at_port(Frame, Port, Choice, ActiveFrame),
    da_stack_trace(Frame, StackFrames),
    maplist(swipl_debug_adapter_translate_stack_frame, [ActiveFrame|StackFrames], StackTrace).

swipl_debug_adapter_translate_stack_frame(stack_frame(Id, InFrameLabel, PI, _Alternative, SourceSpan),
                                          _{ id                          : Id,
                                             name                        : Name,
                                             line                        : SL,
                                             column                      : SC,
                                             endLine                     : EL,
                                             endColumn                   : EC,
                                             source                      : DAPSource,
                                             instructionPointerReference : DAPLabel
                                           }) :-
    term_string(PI, Name),
    swipl_debug_adapter_translate_source_span(SourceSpan, DAPSource, SL, SC, EL, EC),
    swipl_debug_adapter_translate_inframe_label(InFrameLabel, DAPLabel).

swipl_debug_adapter_translate_scope(scope(Name, VariablesRef, SourceSpan),
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
    swipl_debug_adapter_translate_source_span(SourceSpan, DAPSource, SL, SC, EL, EC).

swipl_debug_adapter_translate_inframe_label(port(Port), DAPLabel) :-
    !,
    functor(Port, PortName, _Arity),
    atom_string(PortName, DAPLabel).
swipl_debug_adapter_translate_inframe_label(pc(PC), DAPLabel) :-
    number_string(PC, DAPLabel).


swipl_debug_adapter_translate_step_in_target(step_in_target(Id, null), _{ id    : Id,
                                                                          label : "step" }) :- !.
swipl_debug_adapter_translate_step_in_target(step_in_target(Id, _Alt), _{ id    : Id,
                                                                          label : "fail" }).


prolog:break_hook(Clause, PC, FR, BFR, Expression, Action) :-
    swipl_debug_adapter_source_breakpoint(BP, Clause, PC, Cond, Hit0, Hit, Log),
    swipl_debug_adapter_break_hook(BP, Clause, PC, FR, BFR, Expression, Cond, Hit0, Hit, Log, Action).


:- det(swipl_debug_adapter_break_hook/11).
swipl_debug_adapter_break_hook(BP, Clause, PC, FR, _BFR, _Expression, Cond, Hit, Hit, Log, Action) :-
    !,
    retractall(swipl_debug_adapter_source_breakpoint(BP, _, _, _, _, _, _)),
    asserta(swipl_debug_adapter_source_breakpoint(BP, Clause, PC, Cond, 0, Hit, Log)),
    da_frame_evaluate(FR, Cond, Result, _),
    swipl_debug_adapter_breakpoint_action(BP, FR, Result, Log, Action).
swipl_debug_adapter_break_hook(BP, Clause, PC, _FR, _BFR, _Expression, Cond, Hit0, Hit, Log, continue) :-
    retractall(swipl_debug_adapter_source_breakpoint(BP, _, _, _, _, _, _)),
    Hit1 is Hit0 + 1,
    asserta(swipl_debug_adapter_source_breakpoint(BP, Clause, PC, Cond, Hit1, Hit, Log)).


:- det(swipl_debug_adapter_breakpoint_action/5).
swipl_debug_adapter_breakpoint_action( BP, _FR, true, null, trace   ) :- !,
    retractall(swipl_debug_adapter_last_action(_)),
    asserta(swipl_debug_adapter_last_action(breakpoint(BP))).
swipl_debug_adapter_breakpoint_action(_BP, _FR, _   , null               , continue) :- !.
swipl_debug_adapter_breakpoint_action( BP,  FR, true, log_message(String), continue) :- !,
    da_frame_variables_mapping(FR, Map),
    print_message(trace, log_message(BP, Map, String)).
swipl_debug_adapter_breakpoint_action(_BP, _FR, _   , _                  , continue).


:- det(swipl_debug_adapter_breakpoints_set/3).
swipl_debug_adapter_breakpoints_set(path(Path), Req, Res) :-
    user:ensure_loaded(Path),
    forall(swipl_debug_adapter_breakpoint_path(BP, Path),
           swipl_debug_adapter_breakpoints_delete(BP)),
    phrase(swipl_debug_adapter_breakpoints_set(Req, path(Path)), Res).


swipl_debug_adapter_breakpoints_delete(BP) :-
    catch(ignore(prolog_breakpoints:delete_breakpoint(BP)), _, true),
    retractall(swipl_debug_adapter_source_breakpoint(BP, _, _, _, _, _, _)).


swipl_debug_adapter_breakpoint_path(BP, Path) :-
    prolog_breakpoints:breakpoint_property(BP, file(Path)).


swipl_debug_adapter_breakpoints_set([   ], _) --> [].
swipl_debug_adapter_breakpoints_set([H|T], P) --> swipl_debug_swipl_breakpoint_set(H, P), swipl_debug_adapter_breakpoints_set(T, P).


:- det(swipl_debug_swipl_breakpoint_set/4).
swipl_debug_swipl_breakpoint_set(source_breakpoint(L0, C0, Cond, Hit, Log), path(P)) -->
    {   prolog_breakpoints:set_breakpoint(P, L0, C0, BP)   },
    !,
    {   prolog_breakpoints:known_breakpoint(Clause, PC, _, BP),
        asserta(swipl_debug_adapter_source_breakpoint(BP, Clause, PC, Cond, 0, Hit, Log)),
        prolog_breakpoints:breakpoint_property(BP, character_range(A, L)),
        Z is A + L,
        da_source_file_offsets_line_column_pairs(path(P), [A, Z], [SL-SC, EL-EC])
    },
    [   breakpoint(BP, true, null, span(path(P), SL, SC, EL, EC))   ].
swipl_debug_swipl_breakpoint_set(_, _) --> [].
