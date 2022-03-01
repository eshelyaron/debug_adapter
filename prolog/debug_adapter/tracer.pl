:- module(
       da_tracer,
       [
           da_debugee/4,
           da_terminal/3,
           da_tracer_trapping/0
       ]
   ).

/** <module> SWI-Prolog Debug Adapter Tracer

This module contains various interactions with the runtime
introspection facilities of SWI-Prolog to implement hook-based
tracer which is attached to each DAP debugged thread.
*/

:- use_module(stack).
:- use_module(frame).
:- use_module(source, [qualified/3]).
:- use_module(server, [da_server/1]).

user:prolog_trace_interception(Port, Frame, Choice, Action) :-
    notrace(da_trace_interception(Port, Frame, Choice, Action)),
    da_tracer_yield(Action).

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
    da_debugee_emitted_message(thread_started, ServerThreadId, ServerInterruptHandle),
    thread_get_message(_), % wait for a trigger from the server
    debug(dap(tracer), "starting debugee thread with source file ~w and goal ~w", [ModulePath, Goal]),
    absolute_file_name(ModulePath, AbsModulePath, []),
    debug(dap(tracer), "Absolute path to source file ~w", [AbsModulePath]),
    user:ensure_loaded(AbsModulePath),
    debug(dap(tracer), "Loaded source file ~w", [AbsModulePath]),
    (   module_property(Module, file(AbsModulePath))
    ->  qualified(QGoal, Module, Goal)
    ;   QGoal = Goal
    ),
    debug(dap(tracer), "debugee qualified goal ~w", [QGoal]),
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
    da_tracer_setup(ServerThreadId, ServerInterruptHandle),
    da_tracer_top_level(Goal, ExitCode),
    debug(dap(tracer), "tracer cleanup", []),
    da_debugee_exited(ExitCode, ServerThreadId, ServerInterruptHandle).


da_tracer_top_level(Goal, ExitCode) :-
    catch((   trace, Goal, notrace
          ->  ExitCode = 0
          ;   notrace, ExitCode = 1
          ),
          _Catcher,
          (notrace, ExitCode = 2)
         ).


da_tracer_setup(ServerThreadId, ServerInterruptHandle) :-
    asserta(da_debugee_server(ServerThreadId, ServerInterruptHandle)),
    asserta(da_tracer_last_action(null)),
    asserta((user:thread_message_hook(Term, Kind, Lines) :-
                 da_debugee_emitted_message(output(Term, Kind, Lines),
                                            ServerThreadId,
                                            ServerInterruptHandle),
                 false)),
    asserta((user:prolog_exception_hook(Ex, Out, Frame, Catcher) :-
                 da_exception_hook(Ex, Out, Frame, Catcher))),
    set_prolog_flag(gui_tracer, true),
    visible([+call, +exit, +fail, +redo, +unify, +cut_call, +cut_exit, +exception]),
    prolog_skip_level(_, very_deep).

:- dynamic da_tracer_trapping/0.

da_exception_hook(_In, _Out, _Frame, _Catcher) :-
    thread_self(Me),
    thread_property(Me, debug(true)),
    da_tracer_trapping,
    trace,
    fail.

da_terminal(ServerSocket, ServerThreadId, ServerInterruptHandle) :-
    da_debugee_emitted_message(thread_started, ServerThreadId, ServerInterruptHandle),
    da_terminal_setup(ServerSocket),
    da_tracer_setup(ServerThreadId, ServerInterruptHandle),
    prolog.


da_terminal_setup(ServerSocket) :-
    tcp_listen(ServerSocket, 5),
    tcp_accept(ServerSocket, ClientSocket, _Peer),
    tcp_open_socket(ClientSocket, InStream, OutStream),
    set_stream(InStream, close_on_abort(false)),
    set_stream(OutStream, close_on_abort(false)),
    set_prolog_IO(InStream, OutStream, OutStream),
    set_stream(InStream, tty(true)),
    set_prolog_flag(tty_control, false),
    current_prolog_flag(encoding, Enc),
    set_stream(user_input, encoding(Enc)),
    set_stream(user_output, encoding(Enc)),
    set_stream(user_error, encoding(Enc)),
    set_stream(user_input, newline(detect)),
    set_stream(user_output, newline(dos)),
    set_stream(user_error, newline(dos)),
    set_prolog_flag(toplevel_prompt, '?- ').

:- det(da_debugee_exited/3).
da_debugee_exited(R, S, W) :-
    da_debugee_emitted_message(thread_exited, S, W),
    da_debugee_emitted_message(exited(R), S, W).


%! user:debugger_connection_template(-Template) is nondet.
%
%  Mutlifile, dynamic predicate. Initially undefined.
%
%  When defined by the user, Template must be unified with a string that
%  will be interpolated with `format(string(Command), Template, [TCPPort])`
%  to produce a shell command _Command_ that starts a DAP client and
%  connects to the DAP server via TCP port _TCPPort_.
%
%  This hook is used to start a user specified DAP client when
%  intercepting the tracer for a thread that does not partake in a
%  preexisting DAP session.
%
%  For example, using GNU Emacs with `dap-mode`:
%  ```
%  ?- [library(debug_adapter/tracer)].
%  true.
%
%  ?- asserta(user:debugger_connection_template("emacs --eval '(dap-debug (list :type \"swi-prolog-tcp\" :debugServer ~w))' &")).
%  true.
%
%  ?- ['foo.pl']     % assuming foo.pl contains a definition of foo/0
%  true.
%
%  ?- trace, foo.  % Emacs pops up showing foo.pl with dap-mode connected to the current thread
%  ```

:- multifile user:debugger_connection_template/1.
:- dynamic user:debugger_connection_template/1.

da_trace_interception(Port, Frame, Choice, Action) :-
    da_debugee_server(ServerThreadId, ServerInterruptHandle),
    !,
    da_tracer_last_action(LastAction),
    !,
    retractall(da_tracer_last_action(_)),
    da_tracer_stopped_reason(Port, Frame, LastAction, Reason, Description, Text, BreakpointIds),
    da_debugee_emitted_message(stopped(Reason, Description, Text, BreakpointIds), ServerThreadId, ServerInterruptHandle),
    da_tracer_loop(Port, Frame, Choice, Action, ServerThreadId, ServerInterruptHandle).
da_trace_interception(Port, Frame, Choice, Action) :-
    user:debugger_connection_template(Template),
    !,
    pipe(R, W),
    tcp_socket(ServerSocket),
    tcp_setopt(ServerSocket, reuseaddr),
    tcp_bind(ServerSocket, TCPPort),
    format(string(Command), Template, [TCPPort]),
    tcp_listen(ServerSocket, 5),
    shell(Command),
    tcp_accept(ServerSocket, ClientSocket, _Peer),
    tcp_open_socket(ClientSocket, InStream, OutStream),
    thread_self(Self0),
    thread_property(Self0, id(Self)),
    thread_create(da_server([threads([Self-running]), in(InStream), out(OutStream), interrupt(R-W)]), ServerThreadId),
    da_tracer_setup(ServerThreadId, W),
    da_debugee_emitted_message(stopped("entry", null, null, null), ServerThreadId, W),
    da_tracer_loop(Port, Frame, Choice, Action, ServerThreadId, W).

:- det(da_tracer_loop/6).
da_tracer_loop(Port, Frame, Choice, Action, ServerThreadId, ServerInterruptHandle) :-
    thread_get_message(Message),
    da_tracer_handled_message(Message, Port, Frame, Choice, Action0, ServerThreadId, ServerInterruptHandle),
    (   Action0 == loop
    ->  da_tracer_loop(Port, Frame, Choice, Action, ServerThreadId, ServerInterruptHandle)
    ;   Action  = Action0
    ).


%!  da_tracer_stopped_reason(+Port, +Frame, +LastAction, -Reason, -Description, -Text, -BreakpointIds) is det.

:- det(da_tracer_stopped_reason/7).
da_tracer_stopped_reason(exception(E), _F, _L           , "exception"          , D   , null, null) :- !, term_string(E, D).
da_tracer_stopped_reason(call        , _F, null         , "entry"              , null, null, null) :- !.
da_tracer_stopped_reason(_P          , _F, step_in      , "step"               , null, null, null) :- !.
da_tracer_stopped_reason(_P          , _F, step_out     , "step"               , null, null, null) :- !.
da_tracer_stopped_reason(_P          , _F, next         , "step"               , null, null, null) :- !.
da_tracer_stopped_reason(_P          , _F, restart_frame, "restart"            , null, null, null) :- !.
da_tracer_stopped_reason(_P          , _F, pause        , "pause"              , null, null, null) :- !.
da_tracer_stopped_reason(_P          , _F, breakpoint(B), "breakpoint"         , null, null, [B] ) :- !.
da_tracer_stopped_reason(call        ,  F, _L           , "function breakpoint", null, null, null) :-
    prolog_frame_attribute(F, predicate_indicator, PI),
    da_breakpoint:da_known_function_breakpoint(PI), !.
da_tracer_stopped_reason(_P          , _F, _L           , "trace"              , null, null, null).  % debugee voluntarily started the tracer


:- det(prolog_dap_stopped_reason/5).
prolog_dap_stopped_reason(Port, _, Reason, null, null) :-
    functor(Port, Atom, _),
    atom_string(Atom, Reason).

:- det(da_tracer_handled_message/7).
da_tracer_handled_message(evaluate(RequestId, FrameId, SourceTerm), _Port, _Frame, _Choice, loop, S, W) :-
    !,
    da_frame_evaluate(FrameId, SourceTerm, Result, Bindings),
    da_debugee_emitted_message(evaluate(RequestId, Result, Bindings), S, W).
da_tracer_handled_message(stack_trace(RequestId), Port, Frame, Choice, loop, S, W) :-
    !,
    da_stack_frame_at_port(Frame, Port, Choice, ActiveFrame),
    da_stack_trace(Frame, StackTrace),
    da_debugee_emitted_message(stack_trace(RequestId, [ActiveFrame|StackTrace]), S, W).
da_tracer_handled_message(step_in_targets(RequestId, FrameId), _Port, Frame, Choice, loop, S, W) :-
    !,
    da_frame_step_in_targets(FrameId, Frame, Choice, Targets),
    da_debugee_emitted_message(step_in_targets(RequestId, Targets), S, W).
da_tracer_handled_message(scopes(RequestId, FrameId), Port, ActiveFrameId, _Choice, loop, S, W) :-
    !,
    da_frame_scopes(FrameId, ActiveFrameId, Port, Scopes),
    da_debugee_emitted_message(scopes(RequestId, Scopes), S, W).
da_tracer_handled_message(variables(RequestId, VariablesRef), _Port, _Frame, _Choice, loop, S, W) :-
    !,
    da_referenced_variables(VariablesRef, Variables),
    da_debugee_emitted_message(variables(RequestId, Variables), S, W).
da_tracer_handled_message(exception_info(RequestId), exception(Exception), _Frame, _Choice, loop, S, W) :-
    !,
    da_debugee_emitted_message(exception_info(RequestId, Exception), S, W).
da_tracer_handled_message(step_in(0), _Port, _Frame, _Choice, continue, S, W) :-
    !,
    da_debugee_emitted_message(continued, S, W),
    asserta(da_tracer_last_action(step_in)).
da_tracer_handled_message(step_in(1), _Port, _Frame, _Choice, fail, S, W) :-
    !,
    da_debugee_emitted_message(continued, S, W),
    asserta(da_tracer_last_action(step_in)).
da_tracer_handled_message(step_out, _Port, _Frame, _Choice, up, S, W) :-
    !,
    da_debugee_emitted_message(continued, S, W),
    asserta(da_tracer_last_action(step_out)).
da_tracer_handled_message(disconnect, _Port, _Frame, _Choice, nodebug, S, W) :-
    !,
    da_debugee_emitted_message(continued, S, W),
    asserta(da_tracer_last_action(disconnect)).
da_tracer_handled_message(continue, _Port, _Frame, _Choice, continue, S, W) :-
    !,
    da_debugee_emitted_message(continued, S, W),
    asserta(da_tracer_last_action(continue)).
da_tracer_handled_message(restart_frame(FrameId), _Port, _Frame, _Choice, retry(FrameId), _S, _W) :-
    !,
    asserta(da_tracer_last_action(restart_frame)).
da_tracer_handled_message(next, _Port, _Frame, _Choice, skip, S, W) :-
    !,
    da_debugee_emitted_message(continued, S, W),
    asserta(da_tracer_last_action(next)).
da_tracer_handled_message(configuration_done, _, _, _, loop, _, _).


%! da_tracer_yield(TracerAction) is det.
%
%  Called right before returning TracerAction to the Prolog tracer to
%  adjust its subsequent behaviour.

:- det(da_tracer_yield/1).
da_tracer_yield(skip) :-
    trace.
da_tracer_yield(retry) :-
    prolog_skip_level(_, very_deep),
    trace.
da_tracer_yield(retry(_)) :-
    prolog_skip_level(_, very_deep),
    trace.
da_tracer_yield(fail) :-
    prolog_skip_level(_, very_deep),
    trace.
da_tracer_yield(continue) :-
    da_tracer_last_action(continue),
    !,
    notrace,
    prolog_skip_level(_, very_deep),
    debug.
da_tracer_yield(continue) :-
    prolog_skip_level(_, very_deep),
    trace.
da_tracer_yield(up) :-
    prolog_skip_level(_, very_deep),
    trace.
da_tracer_yield(nodebug).
da_tracer_yield(abort).
da_tracer_yield(ignore) :-
    trace.
