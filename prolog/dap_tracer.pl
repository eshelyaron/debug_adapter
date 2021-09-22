:- module(
       dap_tracer,
       [
           dap_trace_interception/4,
           dap_tracer_start/0
       ]
   ).

:- thread_local dap_tracer_state/1.

dap_tracer_start :-
    set_prolog_flag(gui_tracer, true),
    asserta(dap_tracer_state(entry)).

dap_tracer_next_state(S) :-
    retractall(dap_tracer_state(_)),
    asserta(dap_tracer_state(S)).

:- det(dap_trace_interception/4).
dap_trace_interception(Port, Frame, Choice, Action) :-
    (   thread_self(DebugeeThreadId)
    ->  thread_property(DebugeeThreadId, system_thread_id(ThreadId))
    ;   ThreadId = 0
    ),
    dap_tracer_state(S0),
    (   S0 = entry
    ->  Message = stopped(ThreadId, entry, "Program Started"),
        dap_tracer_next_state(stopped)
    ),
    thread_send_message(dap_server_debugee_queue, Message),
    dap_tracer_interrupt_server,
    dap_trace_interaction(Port, Frame, Choice, Action).

dap_tracer_interrupt_server :-
    put_code(dap_server_debugee_out, 3).

:- det(dap_trace_interaction/4).
dap_trace_interaction(Port, Frame, Choice, Action) :-
    thread_get_message(Message),
    debug(swipl_dap, "Message: ~w, Port: ~w, Frame: ~w, Choice: ~w", [Message, Port, Frame, Choice]),
    dap_tracer_handled_message(Message, Port, Frame, Choice, Action0),
    (   Action0 = trace
    ->  dap_trace_interaction(Port, Frame, Choice, Action)
    ;   Action  = Action0
    ).

:- det(dap_tracer_handled_message/5).
dap_tracer_handled_message(stack_trace(_ThreadId), _Port, Frame, _Choice, trace) :-
    dap_tracer_default_prolog_backtrace_depth(Depth),
    dap_get_prolog_backtrace(Depth, StackFrames0, [frame(Frame), guard(system:'<meta-call>'/1)]),
    dap_tracer_enriched_frames(StackFrames0, StackFrames),
    thread_send_message(dap_server_debugee_queue, stack_trace(StackFrames)),
    dap_tracer_interrupt_server.
dap_tracer_handled_message(scope(FrameId), _Port, _Frame, _Choice, trace) :-
    (   prolog_frame_attribute(FrameId, clause, ClauseRef)
    ->  clause_info(ClauseRef, File, TPos, _),
        term_position_from_to(TPos, F, T),
        file_offset_line_column(File, F, StartLine, StartColumn),
        file_offset_line_column(File, T, EndLine, EndColumn),
        thread_send_message(dap_server_debugee_queue, scope(File, StartLine, StartColumn, EndLine, EndColumn))
    ;   thread_send_message(dap_server_debugee_queue, scope(foreign))
    ),
    dap_tracer_interrupt_server.
dap_tracer_handled_message(arguments(FrameId), _Port, _Frame, _Choice, trace) :-
    (   prolog_frame_attribute(FrameId, clause, ClauseRef)
    ->  clause_info(ClauseRef, File, TPos, _, [variable_names(Variables)]),
        thread_send_message(dap_server_debugee_queue, Variables)
    ;   thread_send_message(dap_server_debugee_queue, scope(foreign))
    ),
    dap_tracer_interrupt_server.

dap_get_prolog_backtrace(MaxDepth, Stack, Options) :-
    (   option(frame(Fr), Options)
    ->  PC = call
    ),
    option(guard(Guard), Options, none),
    dap_backtrace(MaxDepth, Fr, PC, 4, Guard, Stack, Options).

dap_def_no_clause_refs(system:catch_with_backtrace/3).

dap_backtrace( MaxDepth, Fr, PC, GoalDepth, Guard,
                          [frame(Fr, Level, Where, Goal)|Stack],
                          Options
                        ) :-
    prolog_frame_attribute(Fr, level, Level),
    (   PC == foreign
    ->  prolog_frame_attribute(Fr, predicate_indicator, Pred),
        Where = foreign(Pred)
    ;   PC == call
    ->  prolog_frame_attribute(Fr, predicate_indicator, Pred),
        Where = call(Pred)
    ;   prolog_frame_attribute(Fr, clause, Clause)
    ->  ClauseRefs = true,
        clause_where(ClauseRefs, Clause, PC, Where, Options)
    ;   Where = meta_call
    ),
    (   Where == meta_call
    ->  Goal = 0
    ;   copy_goal(GoalDepth, Fr, Goal)
    ),
    (   prolog_frame_attribute(Fr, pc, PC2)
    ->  true
    ;   PC2 = foreign
    ),
    (   prolog_frame_attribute(Fr, parent, Parent),
        prolog_frame_attribute(Parent, predicate_indicator, PI),
        PI == Guard                             % last frame
    ->  dap_backtrace(1, Parent, PC2, GoalDepth, Guard, Stack, Options)
    ;   prolog_frame_attribute(Fr, parent, Parent),
        more_stack(Parent)
    ->  D2 is MaxDepth - 1,
        dap_backtrace(D2, Parent, PC2, GoalDepth, Guard, Stack, Options)
    ;   Stack = []
    ).

more_stack(Parent) :-
    prolog_frame_attribute(Parent, predicate_indicator, PI),
    \+ (   PI = ('$toplevel':G),
           G \== (toplevel_call/1)
       ),
    !.
more_stack(_) :-
    current_prolog_flag(break_level, Break),
    Break >= 1.

clause_where(true, Clause, PC, clause(Clause, PC), _).
clause_where(false, Clause, PC, pred_line(PredName, File:Line), Options) :-
    option(subgoal_positions(true), Options, true),
    subgoal_position(Clause, PC, File, CharA, _CharZ),
    File \= @(_),                 % XPCE Object reference
    lineno(File, CharA, Line),
    clause_predicate_name(Clause, PredName),
    !.
clause_where(false, Clause, _PC, pred_line(PredName, File:Line), _Options) :-
    clause_property(Clause, file(File)),
    clause_property(Clause, line_count(Line)),
    clause_predicate_name(Clause, PredName),
    !.
clause_where(false, Clause, _PC, clause_name(ClauseName), _Options) :-
    clause_name(Clause, ClauseName).

subgoal_position(ClauseRef, PC, File, CharA, CharZ) :-
    debug(backtrace, 'Term-position in ~p at PC=~w:', [ClauseRef, PC]),
    clause_info(ClauseRef, File, TPos, _),
    '$clause_term_position'(ClauseRef, PC, List),
    debug(backtrace, '\t~p~n', [List]),
    find_subgoal(List, TPos, PosTerm),
    arg(1, PosTerm, CharA),
    arg(2, PosTerm, CharZ).

lineno(File, Char, Line) :-
    setup_call_cleanup(
        ( prolog_clause:try_open_source(File, Fd),
          set_stream(Fd, newline(detect))
        ),
        lineno_(Fd, Char, Line),
        close(Fd)).

lineno_(Fd, Char, L) :-
    stream_property(Fd, position(Pos)),
    stream_position_data(char_count, Pos, C),
    C > Char,
    !,
    stream_position_data(line_count, Pos, L0),
    L is L0-1.
lineno_(Fd, Char, L) :-
    skip(Fd, 0'\n),
    lineno_(Fd, Char, L).

copy_goal(0, _, 0) :- !.                        % 0 is not a valid goal
copy_goal(D, Fr, Goal) :-
    prolog_frame_attribute(Fr, goal, Goal0),
    (   Goal0 = Module:Goal1
    ->  copy_term_limit(D, Goal1, Goal2),
        (   hidden_module(Module)
        ->  Goal = Goal2
        ;   Goal = Module:Goal2
        )
    ;   copy_term_limit(D, Goal0, Goal)
    ).

hidden_module(system).
hidden_module(user).

copy_term_limit(0, In, '...') :-
    compound(In),
    !.
copy_term_limit(N, In, Out) :-
    is_dict(In),
    !,
    dict_pairs(In, Tag, PairsIn),
    N2 is N - 1,
    MaxArity = 16,
    copy_pairs(PairsIn, N2, MaxArity, PairsOut),
    dict_pairs(Out, Tag, PairsOut).
copy_term_limit(N, In, Out) :-
    compound(In),
    !,
    compound_name_arity(In, Functor, Arity),
    N2 is N - 1,
    MaxArity = 16,
    (   Arity =< MaxArity
    ->  compound_name_arity(Out, Functor, Arity),
        copy_term_args(0, Arity, N2, In, Out)
    ;   OutArity is MaxArity+2,
        compound_name_arity(Out, Functor, OutArity),
        copy_term_args(0, MaxArity, N2, In, Out),
        SkipArg is MaxArity+1,
        Skipped is Arity - MaxArity - 1,
        format(atom(Msg), '<skipped ~D of ~D>', [Skipped, Arity]),
        arg(SkipArg, Out, Msg),
        arg(Arity, In, InA),
        arg(OutArity, Out, OutA),
        copy_term_limit(N2, InA, OutA)
    ).
copy_term_limit(_, In, Out) :-
    copy_term_nat(In, Out).

copy_term_args(I, Arity, Depth, In, Out) :-
    I < Arity,
    !,
    I2 is I + 1,
    arg(I2, In, InA),
    arg(I2, Out, OutA),
    copy_term_limit(Depth, InA, OutA),
    copy_term_args(I2, Arity, Depth, In, Out).
copy_term_args(_, _, _, _, _).

copy_pairs([], _, _, []) :- !.
copy_pairs(Pairs, _, 0, ['<skipped>'-Skipped]) :-
    !,
    length(Pairs, Skipped).
copy_pairs([K-V0|T0], N, MaxArity, [K-V|T]) :-
    copy_term_limit(N, V0, V),
    MaxArity1 is MaxArity - 1,
    copy_pairs(T0, N, MaxArity1, T).

term_position_from_to(TPos, F, T) :-
    arg(1, TPos, F),
    arg(2, TPos, T).

dap_tracer_default_prolog_backtrace_depth(16).

:- det(dap_tracer_enriched_frames/2).
dap_tracer_enriched_frames([H0|T0], [H|T]) :-
    debug(swipl_dap, "frame: ~w", [H0]),
    dap_tracer_enriched_frame(H0, H),
    dap_tracer_enriched_frames(T0, T).

dap_tracer_enriched_frames([], []).

:- det(dap_tracer_enriched_frame/2).
dap_tracer_enriched_frame(frame(Id, _Level, meta_call, _Goal), stack_frame(Id, meta, call)) :- !.
dap_tracer_enriched_frame(frame(Id, _Level, call(Module:F/A), _Goal), stack_frame(Id, Module, PI, File, Line, 0)) :- !,
    term_string(F/A, PI),
    (   A = 0
    ->  P = F
    ;   functor(P, F, A)
    ),
    (   predicate_property(Module:P, file(File))
    ->  (   predicate_property(Module:P, line_count(Line))
        ->  true
        ;   Line = 0
        )
    ;   File = null, Line = 0
    ).

dap_tracer_enriched_frame(frame(Id, _Level, foreign(Module:PI0), _Goal), stack_frame(Id, Module, PI)) :- !,
    term_string(PI0, PI).
dap_tracer_enriched_frame(frame(Id, _Level, clause(ClauseRef, PC), _Goal), stack_frame(Id, Module, PI, File, Line, Column)) :-
    (   clause_info(ClauseRef, File, TPos, _)
    ->  '$clause_term_position'(ClauseRef, PC, List),
        find_subgoal(List, TPos, PosTerm),
        arg(1, PosTerm, StartOffset),
        file_offset_line_column(File, StartOffset, Line, Column)
    ;   File = null, Line = 0, Column = 0
    ),
    clause_predicate_name(ClauseRef, PI),
    clause_property(ClauseRef, module(Module)).

find_subgoal([A|T], term_position(_, _, _, _, PosL), SPos) :-
    is_list(PosL),
    nth1(A, PosL, Pos),
    nonvar(Pos),
    !,
    find_subgoal(T, Pos, SPos).
find_subgoal(_, Pos, Pos).

clause_predicate_name(Clause, PredName) :-
    user:prolog_clause_name(Clause, PredName),
    !.
clause_predicate_name(Clause, PredName) :-
    nth_clause(Head, _N, Clause),
    !,
    predicate_name(user:Head, PredName).

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
