:- module(
       swipl_debug_adapter,
       [
           swipl_debug_adapter_command_callback/6
       ]
   ).

:- use_module(library(debug_adapter/compat)).
:- use_module(library(debug_adapter/sdk)).
:- use_module(library(debug_adapter/stack)).


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
swipl_debug_adapter_command_callback(stackTrace, Arguments, ReqSeq, _Handle, configured(Threads0), configured(Threads)) :-
    !,
    debug(dap(swipl), "Handling stackTrace request", []),
    _{ threadId : ThreadId } :< Arguments,
    select(ThreadId, Threads0, Threads1),
    catch((thread_send_message(ThreadId, stack_trace(ReqSeq)), Threads = Threads0),
          _,
          Threads = Threads1).
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


:- thread_local
   swipl_debug_adapter_handle/1,
   swipl_debug_adapter_last_action/1,
   swipl_debug_adapter_function_breakpoint/1.


:- det(swipl_debug_adapter_trace/3).
swipl_debug_adapter_trace(QGoal, VarNames, Handle) :-
    asserta(swipl_debug_adapter_handle(Handle)),
    asserta(swipl_debug_adapter_last_action(entry)),
    asserta((user:thread_message_hook(Term, Kind, Lines) :-
                 swipl_debug_adapter_message_hook(Term, Kind, Lines),
                 fail)),
    set_prolog_flag(gui_tracer, true),
    visible([+call, +exit, +fail, +redo, +unify, +cut_call, +cut_exit, +exception]),
    prolog_skip_level(_, very_deep),
    swipl_debug_adapter_goal_reified_result(QGoal, VarNames, Result),
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


:- det(swipl_debug_adapter_message_hook/3).
swipl_debug_adapter_message_hook(_   , silent, _) :- !.
swipl_debug_adapter_message_hook(Term, _     , _) :-
    swipl_debug_adapter_handle(Handle),
    message_to_string(Term, String0),
    string_concat(String0, "\n", String),
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
    swipl_debug_adapter_handle_messages(Port, Frame, Choice, Handle, Action),
    debug(dap(tracer), "Handled messages", []).

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
swipl_debug_adapter_translate_inframe_label(port(Port), DAPLabel) :-
    !,
    functor(Port, PortName, _Arity),
    atom_string(PortName, DAPLabel).
swipl_debug_adapter_translate_inframe_label(pc(PC), DAPLabel) :-
    number_string(PC, DAPLabel).
