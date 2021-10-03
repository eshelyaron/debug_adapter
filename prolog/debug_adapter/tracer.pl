:- module(
       da_tracer,
       [
           da_debugee/4
       ]
   ).

/** <module> SWI-Prolog Debug Adapter Tracer

This module contains various interactions with the runtime
introspection facilities of SWI-Prolog to implement hook-based
tracer which is attached to each DAP debugged thread.
*/

:- use_module(stack).
:- use_module(frame).
:- use_module(clause, [qualified/3]).

:- thread_local da_debugee_server/2.
:- thread_local da_tracer_last_action/1.

:- multifile prolog:open_source_hook/3.

prolog:open_source_hook(Path, Stream, _Options) :-
    (   da_debugee_server(ServerThreadId, ServerInterruptHandle)
    ->  (   source_file(Path)
        ->  da_debugee_emitted_message(loaded_source("new", Path), ServerThreadId, ServerInterruptHandle)
        ;   da_debugee_emitted_message(loaded_source("changed", Path), ServerThreadId, ServerInterruptHandle)
        )
    ;   true
    ),
    open(Path, read, Stream).


% We manually perform custom qualification, so no `meta_predicate` is needed.
% Keep the directive in a comment for reference.
% :- meta_predicate da_debugee(?, 0, ?, ?).

da_debugee(ModulePath, Goal, ServerThreadId, ServerInterruptHandle) :-
    asserta(da_debugee_server(ServerThreadId, ServerInterruptHandle)),
    thread_get_message(_), % wait for a trigger from the server
    absolute_file_name(ModulePath, AbsModulePath, []),
    use_module(AbsModulePath),
    module_property(Module, file(AbsModulePath)),
    qualified(QGoal, Module, Goal),
    da_trace(QGoal, ServerThreadId, ServerInterruptHandle).

da_debugee_emitted_message(Message, ServerThreadId, ServerInterruptHandle) :-
    thread_self(DebugeePrologThreadId),
    thread_property(DebugeePrologThreadId, id(DebugeeSystemThreadId)),
    thread_send_message(ServerThreadId, DebugeeSystemThreadId-Message),
    da_server_interrupt(ServerInterruptHandle).

da_server_interrupt(Handle) :-
    put_code(Handle, 3).

:- meta_predicate da_trace(0, ?, ?).

da_trace(Goal, ServerThreadId, ServerInterruptHandle) :-
    debug(dap(tracer), "tracer setup", []),
    (   current_prolog_flag(gui_tracer, OldFlag)
    ->  true
    ;   OldFlag = false
    ),
    set_prolog_flag(gui_tracer, true),
    asserta((user:prolog_trace_interception(Port, Frame, Choice, Action) :-
                 notrace(da_trace_interception(Port, Frame, Choice, Action))
            ), Ref),
    visible([+unify, +cut_call, +cut_exit, +break]),
    prolog_skip_level(OldSkipLevel, very_deep),
    asserta(da_tracer_last_action(null)),
    trace,
    catch((  Goal
          -> ExitCode = 0
          ;  ExitCode = 1
          ),
          _Catcher,
          ExitCode = 2
         ),
    notrace,
    debug(dap(tracer), "tracer cleanup", []),
    prolog_skip_level(_, OldSkipLevel),
    erase(Ref),

    set_prolog_flag(gui_tracer, OldFlag),
    da_debugee_exited(ExitCode, ServerThreadId, ServerInterruptHandle).

:- det(da_debugee_exited/3).
da_debugee_exited(R, S, W) :-
    da_debugee_emitted_message(thread_exited, S, W),
    da_debugee_emitted_message(exited(R), S, W).

:- det(da_trace_interception/4).
da_trace_interception(Port, Frame, Choice, Action) :-
    debug(dap(tracer), "Intercepting ~w ~w ~w", [Port, Frame, Choice]),
    da_debugee_server(ServerThreadId, ServerInterruptHandle),
    da_tracer_last_action(LastAction),
    retractall(da_tracer_last_action(_)),
    debug(dap(tracer), "Last tracer action was ~w", [LastAction]),
    da_tracer_stopped_reason(Port, LastAction, Reason, Description, Text, BreakpointIds),
    da_debugee_emitted_message(stopped(Reason, Description, Text, BreakpointIds), ServerThreadId, ServerInterruptHandle),
    da_tracer_loop(Port, Frame, Choice, Action, ServerThreadId, ServerInterruptHandle).

da_tracer_loop(Port, Frame, Choice, Action, ServerThreadId, ServerInterruptHandle) :-
    thread_get_message(Message),
    da_tracer_handled_message(Message, Port, Frame, Choice, Action0, ServerThreadId, ServerInterruptHandle),
    (   Action0 == loop
    ->  da_tracer_loop(Port, Frame, Choice, Action, ServerThreadId, ServerInterruptHandle)
    ;   Action  = Action0
    ).


%!  da_tracer_stopped_reason(+Port, +LastAction, -Reason, -Description, -Text, -BreakpointIds) is det.

:- det(da_tracer_stopped_reason/6).
da_tracer_stopped_reason(break(_), _, "breakpoint", null, null, null) :- !.
da_tracer_stopped_reason(exception(Exception), _, "exception", null, Description, null) :-
    !,
    term_string(Exception, Description).
da_tracer_stopped_reason(call, null, "entry", null, null, null) :- !.
da_tracer_stopped_reason(_, step_in, "step", null, null, null) :- !.


:- det(prolog_dap_stopped_reason/5).
prolog_dap_stopped_reason(Port, _, Reason, null, null) :-
    functor(Port, Atom, _),
    atom_string(Atom, Reason).

:- det(da_tracer_handled_message/7).
da_tracer_handled_message(stack_trace(RequestId), Port, Frame, Choice, loop, S, W) :-
    !,
    da_stack_frame_at_port(Frame, Port, Choice, ActiveFrame),
    da_stack_trace(Frame, StackTrace),
    da_debugee_emitted_message(stack_trace(RequestId, [ActiveFrame|StackTrace]), S, W).
da_tracer_handled_message(scopes(RequestId, FrameId), Port, ActiveFrameId, _Choice, loop, S, W) :-
    !,
    da_frame_scopes(FrameId, ActiveFrameId, Port, Scopes),
    da_debugee_emitted_message(scopes(RequestId, Scopes), S, W).
da_tracer_handled_message(variables(RequestId, VariablesRef), _Port, _Frame, _Choice, loop, S, W) :-
    !,
    da_referenced_variables(VariablesRef, Variables),
    da_debugee_emitted_message(variables(RequestId, Variables), S, W).
da_tracer_handled_message(step_in, _Port, _Frame, _Choice, continue, _S, _W) :-
    !,
    asserta(da_tracer_last_action(step_in)).
da_tracer_handled_message(disconnect, _Port, _Frame, _Choice, nodebug, _S, _W) :-
    !,
    asserta(da_tracer_last_action(disconnect)).
da_tracer_handled_message(continue, _Port, _Frame, _Choice, nodebug, _S, _W) :-
    !,
    asserta(da_tracer_last_action(continue)).
da_tracer_handled_message(restart_frame(FrameId), _Port, _Frame, _Choice, retry(FrameId), _S, _W) :-
    !,
    asserta(da_tracer_last_action(restart_frame)).
