:- module(
       swipl_debug_adapter,
       [
           swipl_debug_adapter_command_callback/6
       ]
   ).

:- use_module(library(debug_adapter/sdk)).


swipl_debug_adapter_command_callback(disconnect, _Arguments, ReqSeq, Handle, [], disconnected) :-
    !,
    debug(dap(swipl), "Disconnecting", []),
    da_sdk_response(Handle, ReqSeq, disconnect),
    da_sdk_event(Handle, exited),
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
    da_sdk_event(Handle, exited),
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
swipl_debug_adapter_command_callback(disconnect, _Arguments, ReqSeq, Handle, configured(Threads), disconnected) :-
    !,
    debug(dap(swipl), "disconnecting", []),
    maplist([T]>>thread_send_message(T, disconnect), Threads),
    da_sdk_response(Handle, ReqSeq, disconnect),
    da_sdk_event(Handle, exited),
    da_sdk_stop(Handle).
swipl_debug_adapter_command_callback(continue, Arguments, ReqSeq, Handle, State, State) :-
    !,
    _{ threadId : ThreadId } :< Arguments,
    thread_send_message(ThreadId, continue),
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


swipl_debug_adapter_launch_thread(Args, Handle, PrologThreadId) :-
    _{ cwd: CWD, module: ModulePath, goal: GoalString } :< Args,
    !,
    cd(CWD),
    user:ensure_loaded(ModulePath),
    thread_self(ServerThreadId),
    thread_create(swipl_debug_adapter_debugee(ModulePath, GoalString, ServerThreadId, Handle), PrologThreadId),
    thread_get_message(started(PrologThreadId)).

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


:- thread_local
   swipl_debug_adapter_handle/1,
   swipl_debug_adapter_last_action/1,
   swipl_debug_adapter_function_breakpoint/1.


:- det(swipl_debug_adapter_trace/3).
swipl_debug_adapter_trace(QGoal, _VarNames, Handle) :-
    asserta(swipl_debug_adapter_handle(Handle)),
    asserta(swipl_debug_adapter_last_action(entry)),
    set_prolog_flag(gui_tracer, true),
    visible([+call, +exit, +fail, +redo, +unify, +cut_call, +cut_exit, +exception]),
    prolog_skip_level(_, very_deep),
    debug(dap(tracer), "Debugee qualified goal ~w", [QGoal]),
    trace, QGoal, notrace.


qualified(Module:UnqualifiedGoal, Module, UnqualifiedGoal) :-
    !.
qualified('<meta-call>'(_Module0:Goal), Module, UnqualifiedGoal) :-
    qualified(Goal, Module, UnqualifiedGoal),
    !.
qualified('<meta-call>'(Goal), Module, UnqualifiedGoal) :-
    qualified(Goal, Module, UnqualifiedGoal),
    !.
qualified(UnqualifiedGoal, user, UnqualifiedGoal).


user:prolog_trace_interception(Port, Frame, Choice, Action) :-
    notrace(swipl_debug_adapter_trace_interception(Port, Frame, Choice, Action)),
    debug(dap(tracer), "Trace action ~w", [Action]),
    swipl_debug_adapter_tracer_yield(Action).


swipl_debug_adapter_trace_interception(Port, Frame, Choice, Action) :-
    debug(dap(tracer), "Trace interception", []),
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
    debug(dap(tracer), "Got message ~w", [M]),
    swipl_debug_adapter_handle_message(M, Port, Frame, Choice, Handle, Action).

swipl_debug_adapter_handle_message(continue, _Port, _Frame, _Choice, Handle, continue) :-
    !,
    thread_self(Self),
    thread_property(Self, id(Id)),
    da_sdk_event(Handle, continued, _{threadId:Id}),
    retractall(swipl_debug_adapter_last_action(_)),
    asserta(swipl_debug_adapter_last_action(continue)).
swipl_debug_adapter_handle_message(disconnect, _Port, _Frame, _Choice, Handle, nodebug) :-
    !,
    thread_self(Self),
    thread_property(Self, id(Id)),
    da_sdk_event(Handle, continued, _{threadId:Id}),
    retractall(swipl_debug_adapter_last_action(_)),
    asserta(swipl_debug_adapter_last_action(continue)).

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
    swipl_debug_adapter_function_breakpoint(PI),
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
    swipl_debug_adapter_tracer_yield(continue),
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
