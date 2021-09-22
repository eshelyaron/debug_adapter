:- module(
       da_tracer,
       [
           da_debugee/4
       ]
   ).

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

%:- meta_predicate da_debugee(?, 0, ?, ?).

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
    debug(swipl_dap, "tracer setup", []),
    (   current_prolog_flag(gui_tracer, OldFlag)
    ->  true
    ;   OldFlag = false
    ),
    set_prolog_flag(gui_tracer, true),
    asserta((user:prolog_trace_interception(Port, Frame, Choice, Action) :-
                 debug(swipl_dap, "intercepting ~w ~w", [Port, Frame]),
                 da_trace_interception(Port, Frame, Choice, Action)
            ), Ref),
    trace,
    catch((  Goal
          -> ExitCode = 0
          ;  ExitCode = 1
          ),
          _Catcher,
          ExitCode = 2
         ),
    notrace,
    erase(Ref),
    debug(swipl_dap, "tracer cleanup", []),
    set_prolog_flag(gui_tracer, OldFlag),
    da_debugee_exited(ExitCode, ServerThreadId, ServerInterruptHandle).

:- det(da_debugee_exited/3).
da_debugee_exited(R, S, W) :-
    da_debugee_emitted_message(thread_exited, S, W),
    da_debugee_emitted_message(exited(R), S, W).

:- det(da_trace_interception/4).
da_trace_interception(Port, Frame, Choice, Action) :-
    once(da_debugee_server(ServerThreadId, ServerInterruptHandle)),
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
    (   Action0 = loop
    ->  da_tracer_loop(Port, Frame, Choice, Action, ServerThreadId, ServerInterruptHandle)
    ;   Action  = Action0
    ).

:- det(prolog_dap_stopped_reason/5).
prolog_dap_stopped_reason(Port, _, Reason, null, null) :-
    functor(Port, Atom, _),
    atom_string(Atom, Reason).

:- det(da_tracer_handled_message/7).
da_tracer_handled_message(stack_trace(RequestId), _Port, Frame, _Choice, loop, S, W) :-
    current_prolog_flag(backtrace_depth, Depth),
    da_stack_frames(Depth, Frame, call, StackFrames),
    da_debugee_emitted_message(stack_trace(RequestId, StackFrames), S, W).
da_tracer_handled_message(step_in, _Port, _Frame, _Choice, continue, _S, _W).

da_stack_frames(0, _, _, []) :- !.
da_stack_frames(Depth, F, PC, Frames) :-
    (   prolog_frame_attribute(F, hidden, true)
    ->  RestFrames = Frames,
        ND is Depth
    ;   da_stack_frames_source_span(F, PC, SourceRef, Path, SL, SC, EL, EC),
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
    ->  da_stack_frames(ND, Parent, PCParent, RestFrames)
    ;   RestFrames = []
    ).

da_stack_frames_source_span(Frame, PC, Ref, Path, SL, SC, EL, EC) :-
    (   clause_position(PC),
        prolog_frame_attribute(Frame, clause, ClauseRef),
        ClauseRef \== 0
    ->  Ref = 0,
        subgoal_position(ClauseRef, PC, Path, SO, EO),
        file_offset_line_column(Path, SO, SL, SC),
        file_offset_line_column(Path, EO, EL, EC)
    ;   prolog_frame_attribute(Frame, goal, Goal),
        qualify(Goal, QGoal),
        (   predicate_property(QGoal, foreign)
        ->  Ref = 0, Path = null, SL = 0, SC = 0, EL = null, EC = null
        ;   (   clause(QGoal, _Body, ClauseRef)
            ->  Ref = 0,
                subgoal_position(ClauseRef, unify, Path, SO, EO),
                file_offset_line_column(Path, SO, SL, SC),
                file_offset_line_column(Path, EO, EL, EC)
            ;   functor(Goal, Functor, Arity),
                functor(GoalTemplate, Functor, Arity),
                qualify(GoalTemplate, QGoalTemplate),
                clause(QGoalTemplate, _Body, ClauseRef)
            ->  Ref = 0,
                subgoal_position(ClauseRef, unify, Path, SO, EO),
                file_offset_line_column(Path, SO, SL, SC),
                file_offset_line_column(Path, EO, EL, EC)
            ;   predicate_property(QGoal, file(Path)),
                predicate_property(QGoal, line_count(SL))
            ->  Ref = 0, SC = 0, EL = null, EC = null
            ;   da_tracer_cached_goal(QGoal, Ref), Path = null, SL = 0, SC = 0, EL = null, EC = null
            )
        )
    ).

:- thread_local da_tracer_goal_ref/2.

da_tracer_cached_goal(Goal, Ref) :-
    (   da_tracer_goal_ref(_, Ref0)
    ->  Ref is Ref0 + 1
    ;   Ref is 0
    ),
    asserta(da_tracer_goal_ref(Goal, Ref)).

subgoal_position(ClauseRef, unify, File, CharA, CharZ) :-
    !,
    clause_info(ClauseRef, File, TPos, _),
    head_pos(ClauseRef, TPos, PosTerm),
    nonvar(PosTerm),
    arg(1, PosTerm, CharA),
    arg(2, PosTerm, CharZ).
subgoal_position(ClauseRef, choice(CHP), File, CharA, CharZ) :-
    !,
    (   prolog_choice_attribute(CHP, type, jump),
        prolog_choice_attribute(CHP, pc, To)
    ->  subgoal_position(ClauseRef, To, File, CharA, CharZ)
    ;   clause_end(ClauseRef, File, CharA, CharZ)
    ).
subgoal_position(ClauseRef, Port, File, CharA, CharZ) :-
    end_port(Port),
    !,
    clause_end(ClauseRef, File, CharA, CharZ).
subgoal_position(ClauseRef, PC, File, CharA, CharZ) :-
    clause_info(ClauseRef, File, TPos, _),
    (   '$clause_term_position'(ClauseRef, PC, List)
    ->  (   find_subgoal(List, TPos, PosTerm)
        ->  true
        ;   PosTerm = TPos,
            fail
        ),
        nonvar(PosTerm),
        arg(1, PosTerm, CharA),
        arg(2, PosTerm, CharZ)
    ;   fail
    ).

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

clause_position(PC) :- integer(PC), !.
clause_position(exit).
clause_position(unify).
clause_position(choice(_)).

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
        line_position(Fd, Column0),
        succ(Column0, Column)
    ;   get_code(Fd, _),
        file_offset_line_column_(Fd, Offset, Line, Column)
    ).
