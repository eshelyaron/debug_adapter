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
    qualify(Goal, Module, QGoal),
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
    trace,
    catch((  Goal
          -> ExitCode = 0
          ;  ExitCode = 1
          ),
          _Catcher,
          ExitCode = 2
         ),
    notrace,
    prolog_skip_level(_, OldSkipLevel),
    erase(Ref),
    debug(dap(tracer), "tracer cleanup", []),
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
    (   da_tracer_last_action(LastAction)
    ->  prolog_dap_stopped_reason(Port, LastAction, Reason, Description, Text)
    ;   Reason = "entry", Description = "Paused on goal entry", Text = null
    ),
    da_debugee_emitted_message(stopped(Reason, Description, Text, null), ServerThreadId, ServerInterruptHandle),
    da_tracer_loop(Port, Frame, Choice, Action, ServerThreadId, ServerInterruptHandle),
    retractall(da_tracer_last_action(_)),
    asserta(da_tracer_last_action(Action)).

da_tracer_loop(Port, Frame, Choice, Action, ServerThreadId, ServerInterruptHandle) :-
    thread_get_message(Message),
    da_tracer_handled_message(Message, Port, Frame, Choice, Action0, ServerThreadId, ServerInterruptHandle),
    (   Action0 == loop
    ->  da_tracer_loop(Port, Frame, Choice, Action, ServerThreadId, ServerInterruptHandle)
    ;   Action  = Action0
    ).

:- det(prolog_dap_stopped_reason/5).
prolog_dap_stopped_reason(Port, _, Reason, null, null) :-
    functor(Port, Atom, _),
    atom_string(Atom, Reason).

:- det(da_tracer_handled_message/7).
da_tracer_handled_message(stack_trace(RequestId), Port, Frame, _Choice, loop, S, W) :-
    !,
    current_prolog_flag(backtrace_depth, Depth),
    da_stack_frames(Depth, Frame, Port, StackFrames),
    da_debugee_emitted_message(stack_trace(RequestId, StackFrames), S, W).
da_tracer_handled_message(scopes(RequestId, FrameId), Port, _Frame, _Choice, loop, S, W) :-
    !,
    da_scopes(FrameId, Port, Scopes),
    da_debugee_emitted_message(scopes(RequestId, Scopes), S, W).
da_tracer_handled_message(variables(RequestId, VariablesRef), Port, _Frame, _Choice, loop, S, W) :-
    !,
    debug(dap(tracer), "Handling message variables(~w, ~w) at port ~w", [RequestId, VariablesRef, Port]),
    da_variables(VariablesRef, Port, Variables),
    debug(dap(tracer), "got Variables = ~w", [Variables]),
    da_debugee_emitted_message(variables(RequestId, Variables), S, W).
da_tracer_handled_message(step_in, _Port, _Frame, _Choice, continue, _S, _W) :- !.
da_tracer_handled_message(disconnect, _Port, _Frame, _Choice, nodebug, _S, _W) :- !.
da_tracer_handled_message(continue, _Port, _Frame, _Choice, nodebug, _S, _W) :- !.
da_tracer_handled_message(restart_frame(FrameId), _Port, _Frame, _Choice, retry(FrameId), _S, _W) :- !.

:- det(da_variables/3).
da_variables(VariablesRef, Port, Variables) :-
    !,
    debug(dap(tracer), "Parsing VariablesRef", []),
    da_variables_reference_type_frame(VariablesRef, Type, Frame),
    debug(dap(tracer), "got ~w ~w", [Type, Frame]),
    (   Type == cached
    ->  da_tracer_compound_arguments(Variables0, Frame),
        indexed_arguments(1, Variables0, Variables)
    ;   da_frame_clause(Frame, ClauseRef),
        debug(dap(tracer), "Frame has clause", []),
        (   clause_info(ClauseRef, _, _, VarNames)
        ->  debug(dap(tracer), "clause has varnames", []),
            da_variables_(Type, Frame, Port, VarNames, Variables)
        ;   Variables = []  % TODO - fetch VarNames via decompilation for dynamic code
        )
    ).

indexed_arguments(_, [], []) :- !.
indexed_arguments(I0, [H0|T0], [variable(Name, H, ChildrenReference)|T]) :-
    indexed_argument_name(I0, Name),
    da_term_factorized(H0, ChildrenReference, H),
    succ(I0, I1),
    indexed_arguments(I1, T0, T).

da_variables_(arguments, Frame, Port, VarNames, Variables) :-
    !,
    prolog_frame_attribute(Frame, predicate_indicator, PI),
    debug(dap(tracer), "Frame has predicate_indicator", []),
    unqualify(PI, _Functor/Arity),
    debug(dap(tracer), "Frame has arity", []),
    da_arguments(Frame, Port, 1, Arity, VarNames, Variables).

da_arguments(Frame, Port, I, Arity, VarNames, [variable(Name, Value, ChildrenReference)|T]) :-
    debug(dap(tracer), "~w =< ~w", [I, Arity]),
    I =< Arity,
    !,
    arg(I, VarNames, Name0),
    debug(dap(tracer), "Arg ~w has name ~w", [I, Name0]),
    (   Name0 == '_'
    ->  indexed_argument_name(I, Name)
    ;   Name = Name0
    ),
    debug(dap(tracer), "Arg ~w has name ~w", [I, Name]),
    prolog_frame_attribute(Frame, argument(I), Value0),
    debug(dap(tracer), "Arg ~w has value ~w", [I, Value0]),
    da_term_factorized(Value0, ChildrenReference, Value),
    debug(dap(tracer), "Value is ~w(~w)", [Value, ChildrenReference]),
    NI is I + 1,
    da_arguments(Frame, Port, NI, Arity, VarNames, T).
da_arguments(_, _, _, _, _, []).

indexed_argument_name(I, N) :-
    format(atom(N), "Argument #~w", [I]).

da_term_factorized(Var, 0, '_') :-
    var(Var),
    !.
da_term_factorized(Compound, Ref, Name) :-
    compound(Compound),
    !,
    compound_name_arguments(Compound, Functor, Arguments),
    length(Arguments, Arity),
    format(atom(Name), '~w/~w', [Functor, Arity]),
    da_tracer_cached_compound_arguments(Arguments, Ref0),
    Ref is (Ref0 << 2) + 2.
da_term_factorized(Term0, 0, Term) :-
    term_string(Term0, Term).

:- thread_local da_tracer_compound_arguments/2.

da_tracer_cached_compound_arguments(Arguments, Ref) :-
    (   da_tracer_compound_arguments(_, Ref0)
    ->  succ(Ref0, Ref)
    ;   Ref = 1
    ),
    asserta(da_tracer_compound_arguments(Arguments, Ref)).

da_variables_reference_type_frame(VariablesRef, cached, CachedRef) :-
    2 is VariablesRef /\ 3,
    !,
    CachedRef is VariablesRef >> 2.
da_variables_reference_type_frame(VariablesRef, locals, Frame) :-
    1 is VariablesRef /\ 3,
    !,
    Frame is VariablesRef >> 2.
da_variables_reference_type_frame(VariablesRef, arguments, Frame) :-
    0 is VariablesRef /\ 3,
    !,
    Frame is VariablesRef >> 2.


:- det(da_scopes/3).
da_scopes(Frame, unify, [scope("Arguments", ArgumentsRef, SourceRef, Path, SL, SC, EL, EC)]) :-
    !,
    da_frame_arguments_reference(Frame, ArgumentsRef),
    prolog_frame_attribute(Frame, clause, ClauseRef),
    da_clause_source_span(ClauseRef, neck, SourceRef, Path, SL, SC, EL, EC).
da_scopes(_Frame, _Port, []).

da_frame_arguments_reference(Frame, Ref) :-
    Ref is (Frame << 2) + 0.

da_frame_locals_reference(Frame, Ref) :-
    Ref is (Frame << 2) + 1.

:- det(da_stack_frames/4).
da_stack_frames(Depth, F, Port, Frames) :-
    (   prolog_frame_attribute(F, hidden, true), debug(dap(tracer), "skipping hidden frame ~w", [F])
    ->  RestFrames = Frames,
        ND is Depth
    ;   da_frame_port_source_span(F, Port, SourceRef, Path, SL, SC, EL, EC),
        prolog_frame_attribute(F, predicate_indicator, PI0),
        term_string(PI0, PI),
        Frames = [stack_frame(F, PI, SourceRef, Path, SL, SC, EL, EC)|RestFrames],
        ND is Depth - 1
    ),
    (   prolog_frame_attribute(F, parent, Parent),
        (   prolog_frame_attribute(F, pc, PCParent)
        ->  true
        ;   PCParent = foreign
        )
    ->  da_ancestral_stack_frames(ND, Parent, PCParent, RestFrames)
    ;   RestFrames = []
    ).

:- det(da_ancestral_stack_frames/4).
da_ancestral_stack_frames(Depth, F, PC, Frames) :-
    (   prolog_frame_attribute(F, hidden, true), debug(dap(tracer), "skipping hidden frame ~w", [F])
    ->  RestFrames = Frames,
        ND is Depth
    ;   da_frame_pc_source_span(F, PC, SourceRef, Path, SL, SC, EL, EC),
        prolog_frame_attribute(F, predicate_indicator, PI0),
        term_string(PI0, PI),
        Frames = [stack_frame(F, PI, SourceRef, Path, SL, SC, EL, EC)|RestFrames],
        ND is Depth - 1
    ),
    (   prolog_frame_attribute(F, parent, Parent),
        (   prolog_frame_attribute(F, pc, PCParent)
        ->  true
        ;   PCParent = foreign
        )
    ->  da_ancestral_stack_frames(ND, Parent, PCParent, RestFrames)
    ;   RestFrames = []
    ).

:- det(da_frame_pc_source_span/8).
da_frame_pc_source_span(Frame, foreign, Ref, Path, SL, SC, EL, EC) :-
    prolog_frame_attribute(Frame, goal, Goal),
    da_foreign_goal_source_span(Goal, Ref, Path, SL, SC, EL, EC), !.
da_frame_pc_source_span(Frame, PC, Ref, Path, SL, SC, EL, EC) :-
    prolog_frame_attribute(Frame, clause, ClauseRef),
    da_call_site_source_span(ClauseRef, PC, Ref, Path, SL, SC, EL, EC), !.

:- det(da_frame_port_source_span/8).
da_frame_port_source_span(Frame, Port, Ref, Path, SL, SC, EL, EC) :-
    da_port_clause_direction(Port, Direction),
    debug(dap(tracer), "da_frame_port_source_span(~w, ~w[~w], ...", [Frame, Port, Direction]),
    !,
    (   prolog_frame_attribute(Frame, pc, PC),
        debug(dap(tracer), "prolog_frame_attribute(~w, pc, ~w)", [Frame, PC])
    ->  prolog_frame_attribute(Frame, parent, ParentFrame),
        debug(dap(tracer), "prolog_frame_attribute(~w, parent, ~w)", [Frame, ParentFrame]),
        (   da_hidden_frame(ParentFrame)
        ->  da_frame_clause_or_goal_source_span(Frame, Direction, Ref, Path, SL, SC, EL, EC)
        ;   prolog_frame_attribute(ParentFrame, clause, ParentClause),
            da_call_site_source_span(ParentClause, PC, Direction, Ref, Path, SL, SC, EL, EC)
        )
    ;   da_frame_clause_or_goal_source_span(Frame, Direction, Ref, Path, SL, SC, EL, EC)
    ).
da_frame_port_source_span(Frame, Port, Ref, Path, SL, SC, EL, EC) :-
    da_port_pc(Port, PC),
    debug(dap(tracer), "da_frame_port_source_span(~w, redo(~w), ...", [Frame, PC]),
    !,
    prolog_frame_attribute(Frame, clause, ClauseRef),
    da_call_site_source_span(ClauseRef, PC, Ref, Path, SL, SC, EL, EC).
da_frame_port_source_span(Frame, unify, Ref, Path, SL, SC, EL, EC) :-
    debug(dap(tracer), "da_frame_port_source_span(~w, unify, ...", [Frame]),
    !,
    da_frame_clause_or_goal_source_span(Frame, neck, Ref, Path, SL, SC, EL, EC).

da_port_pc(redo(PC), PC) :-  !.
da_port_pc(break(PC), PC) :- !.
da_port_pc(cut_call(PC), PC) :- !.
da_port_pc(cut_exit(PC), PC) :- !.

da_port_clause_direction(Port, entry) :- da_entry_port(Port), !.
da_port_clause_direction(Port, exit) :- da_exit_port(Port), !.

da_entry_port(redo(0)) :- !.
da_entry_port(call) :- !.

da_exit_port(fail) :- !.
da_exit_port(exit) :- !.
da_exit_port(exception(_)) :- !.

:- det(da_clause_source_span/8).
da_clause_source_span(ClauseRef, Direction, 0, Path, SL, SC, EL, EC) :-
    debug(dap(tracer), "da_clause_head_source_span(~w, ~w, ...", [ClauseRef, Direction]),
    clause_info(ClauseRef, Path, TPos, Vars),
    !,
    debug(dap(tracer), "clause_info(~w, ~w, ~w, ~w)", [ClauseRef, Path, TPos, Vars]),
    da_clause_static_source_span(ClauseRef, TPos, Direction, Path, SL, SC, EL, EC).
da_clause_source_span(ClauseRef, Direction, Ref, Path, SL, SC, EL, EC) :-
    da_clause_dynamic_source_span(ClauseRef, Direction, Ref, Path, SL, SC, EL, EC).

:- det(da_clause_dynamic_source_span/8).
da_clause_dynamic_source_span(ClauseRef, Direction, 0, Path, SL, SC, EL, EC) :-
    ( clause_property(ClauseRef, fact)
    -> Fact = true
    ;  Fact = false
    ),
    clause_tmp_stream(ClauseRef, Path, R),
    read_term(R, _T, [subterm_positions(TPos)]),
    da_clause_dynamic_source_span_(Fact, TPos, Direction, Path, SL, SC, EL, EC).

da_clause_dynamic_source_span_(true, HeadPos, entry, Path, SL, SC, EL, EC) :-
    !,
    arg(1, HeadPos, SO),
    file_offset_line_column(Path, SO, SL, SC),
    arg(2, HeadPos, EO),
    file_offset_line_column(Path, EO, EL, EC).
da_clause_dynamic_source_span_(_, term_position(_, _, _, _, [HeadPos]), entry, Path, SL, SC, EL, EC) :-
    !,
    arg(1, HeadPos, SO),
    file_offset_line_column(Path, SO, SL, SC),
    arg(2, HeadPos, EO),
    file_offset_line_column(Path, EO, EL, EC).
da_clause_dynamic_source_span_(_, TermPos, exit, Path, SL, SC, SL, EC) :-
    !,
    arg(2, TermPos, EO),
    file_offset_line_column(Path, EO, SL, SC),
    succ(SC, EC).
da_clause_dynamic_source_span_(true, TermPos, neck, Path, SL, SC, SL, EC) :-
    !,
    arg(2, TermPos, EO),
    file_offset_line_column(Path, EO, SL, SC),
    succ(SC, EC).
da_clause_dynamic_source_span_(_, term_position(_F, _T, SO, EO, _), neck, Path, SL, SC, EL, EC) :-
    !,
    file_offset_line_column(Path, SO, SL, SC),
    file_offset_line_column(Path, EO, EL, EC).


:- det(da_clause_static_source_span/8).
da_clause_static_source_span(ClauseRef, TPos, entry, Path, SL, SC, EL, EC) :-
    !,
    head_pos(ClauseRef, TPos, PosTerm),
    arg(1, PosTerm, SO),
    file_offset_line_column(Path, SO, SL, SC),
    arg(2, PosTerm, EO),
    file_offset_line_column(Path, EO, EL, EC).
da_clause_static_source_span(_ClauseRef, TPos, exit, Path, SL, SC, SL, EC) :-
    !,
    arg(2, TPos, EO),
    file_offset_line_column(Path, EO, SL, SC),
    succ(SC, EC).
da_clause_static_source_span(_ClauseRef, term_position(_F, _T, SO, EO, _), neck, Path, SL, SC, EL, EC) :-
    !,
    file_offset_line_column(Path, SO, SL, SC),
    file_offset_line_column(Path, EO, EL, EC).

da_frame_clause_or_goal_source_span(Frame, Direction, Ref, Path, SL, SC, EL, EC) :-
    debug(dap(tracer), "da_frame_clause_or_goal_source_span(~w, ~w, ...", [Frame, Direction]),
    (   prolog_frame_attribute(Frame, clause, ClauseRef),
        debug(dap(tracer), "prolog_frame_attribute(~w, clause, ~w)", [Frame, ClauseRef])
    ->  da_clause_source_span(ClauseRef, Direction, Ref, Path, SL, SC, EL, EC)
    ;   prolog_frame_attribute(Frame, goal, Goal),
        unqualify(Goal, Module, UGoal),
        debug(dap(tracer), "prolog_frame_attribute(~w, goal, ~w:~w)", [Frame, Module, UGoal]),
        (   predicate_property(Module:UGoal, foreign)
        ->  da_foreign_goal_source_span(Module:UGoal, Direction, Ref, Path, SL, SC, EL, EC)
        ;   (   clause(Module:UGoal, _Body, ClauseRef)
            ->  da_clause_source_span(ClauseRef, Direction, Ref, Path, SL, SC, EL, EC)
            ;   functor(UGoal, Functor, Arity),
                functor(UGoalTemplate, Functor, Arity),
                clause(Module:UGoalTemplate, _Body, ClauseRef)
            ->  da_clause_source_span(ClauseRef, Direction, Ref, Path, SL, SC, EL, EC)
            ;   da_predicate_source_span(Module:UGoal, Direction, Ref, Path, SL, SC, EL, EC)
            ->  true
            ;   da_foreign_goal_source_span(Module:UGoal, Direction, Ref, Path, SL, SC, EL, EC)
            )
        )
    ).

da_frame_clause(Frame, ClauseRef) :-
    debug(dap(tracer), "da_frame_clause(~w, ClauseRef)", [Frame]),
    (   prolog_frame_attribute(Frame, clause, ClauseRef)
    ->  true
    ;   prolog_frame_attribute(Frame, goal, Goal),
        unqualify(Goal, Module, UGoal),
        debug(dap(tracer), "prolog_frame_attribute(~w, goal, ~w:~w)", [Frame, Module, UGoal]),
        (   predicate_property(Module:UGoal, foreign)
        ->  false
        ;   (   clause(Module:UGoal, _Body, ClauseRef)
            ->  true
            ;   functor(UGoal, Functor, Arity),
                functor(UGoalTemplate, Functor, Arity),
                clause(Module:UGoalTemplate, _Body, ClauseRef)
            )
        )
    ).

da_predicate_source_span(Goal, _Direction, 0, Path, SL, 0, null, null) :-
    predicate_property(Goal, file(Path)),
    predicate_property(Goal, line(SL)).

da_hidden_frame(Frame) :- prolog_frame_attribute(Frame, hidden, true), !.
da_hidden_frame(Frame) :-
    prolog_frame_attribute(Frame, goal, Goal),
    da_hidden_predicate(Goal), !.

da_hidden_predicate(Goal) :- predicate_property(Goal, nodebug), !.
da_hidden_predicate(Goal) :- predicate_property(Goal, foreign), !.

da_foreign_goal_source_span(Goal, Ref, Path, SL, SC, EL, EC) :-
    da_foreign_goal_source_span(Goal, entry, Ref, Path, SL, SC, EL, EC).

da_foreign_goal_source_span(_Goal, _Direction, 0, null, 0, 0, null, null).

da_call_site_source_span(ClauseRef, PC, Ref, Path, SL, SC, EL, EC) :-
    da_call_site_source_span(ClauseRef, PC, entry, Ref, Path, SL, SC, EL, EC).

da_call_site_source_span(ClauseRef, PC, Direction, 0, Path, SL, SC, EL, EC) :-
    clause_info(ClauseRef, Path, TPos, _),
    '$clause_term_position'(ClauseRef, PC, List),
    !,
    find_subgoal(List, TPos, PosTerm),
    da_call_site_static_source_span(Direction, PosTerm, Path, SL, SC, EL, EC).
da_call_site_source_span(ClauseRef, PC, Direction, 0, Path, SL, SC, EL, EC) :-
    clause_tmp_stream(ClauseRef, Path, R),
    read_term(R, _T, [subterm_positions(TPos)]),
    '$clause_term_position'(ClauseRef, PC, List),
    find_subgoal(List, TPos, PosTerm),
    da_call_site_static_source_span(Direction, PosTerm, Path, SL, SC, EL, EC).

da_call_site_static_source_span(entry, PosTerm, Path, SL, SC, EL, EC) :-
    !,
    arg(1, PosTerm, SO),
    file_offset_line_column(Path, SO, SL, SC),
    arg(2, PosTerm, EO),
    file_offset_line_column(Path, EO, EL, EC).
da_call_site_static_source_span(exit, PosTerm, Path, SL, SC, SL, EC) :-
    !,
    arg(2, PosTerm, EO),
    file_offset_line_column(Path, EO, SL, SC),
    succ(SC, EC).

clause_tmp_stream(ClauseRef, Path, R) :-
    clause(Head, Body, ClauseRef),
    tmp_file_stream(Path, S, [ encoding(text),
                               extension("pl")
                             ]
                   ),
    set_stream(S, buffer(false)),
    portray_clause(S, (Head :- Body)),
    close(S),
    open(Path, read, R).

clause_end(ClauseRef, File, CharA, CharZ) :-
    clause_info(ClauseRef, File, TPos, _),
    nonvar(TPos),
    arg(2, TPos, CharA),
    CharZ is CharA + 1.

end_port(exit).
end_port(fail).
end_port(exception).

head_pos(Ref, Pos, HPos) :-
    clause_property(Ref, fact),
    !,
    HPos = Pos.
head_pos(_, term_position(_, _, _, _, [HPos,_]), HPos).

find_subgoal(_, Pos, Pos) :-
    var(Pos),
    !.
find_subgoal([A|T], term_position(_, _, _, _, PosL), SPos) :-
    nth1(A, PosL, Pos),
    !,
    find_subgoal(T, Pos, SPos).
find_subgoal([1|T], brace_term_position(_,_,Pos), SPos) :-
    !,
    find_subgoal(T, Pos, SPos).
find_subgoal(List, parentheses_term_position(_,_,Pos), SPos) :-
    !,
    find_subgoal(List, Pos, SPos).
find_subgoal(_, Pos, Pos).

qualify(G, Q) :-
    qualify(G, user, Q).

qualify(Goal, _, Goal) :-
    functor(Goal, :, 2),
    !.
qualify(Goal, Module, Module:Goal).

unqualify(Q, U) :-
    unqualify(Q, _, U).

unqualify(Module:Goal, Module, Goal) :-
    !.
unqualify(Goal, user, Goal).

file_offset_line_column(File, Offset, Line, Column) :-
    setup_call_cleanup(
        ( prolog_clause:try_open_source(File, Fd),
          set_stream(Fd, newline(detect))
        ),
        file_offset_line_column_(Fd, Offset, Line, Column),
        close(Fd)).

file_offset_line_column_(Fd, Offset, Line, Column) :-
    character_count(Fd, CurrentOffset),
    (   CurrentOffset == Offset
    ->  line_count(Fd, Line),
        line_position(Fd, Column)
    ;   get_code(Fd, _),
        file_offset_line_column_(Fd, Offset, Line, Column)
    ).
