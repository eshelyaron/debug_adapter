:- module(
       dap_tracer,
       [
           dap_trace_interception/4,
           dap_tracer_start/0
       ]
   ).

:- thread_local dap_tracer_state/1.

dap_tracer_log(M) :-
    open("/tmp/log.log", append, W),
    format(W, "~w~n", [M]),
    flush_output(W),
    close(W).

dap_tracer_start :-
    set_prolog_flag(gui_tracer, true),
    asserta(dap_tracer_state(entry)).

dap_tracer_next_state(S) :-
    retractall(dap_tracer_state(_)),
    asserta(dap_tracer_state(S)).

dap_trace_interception(Port, Frame, Choice, Action) :-
    dap_tracer_log("dap_trace_interception"),
    ( thread_self(DebugeeThreadId) ; DebugeeThreadId = null  ),
    dap_tracer_log(DebugeeThreadId),
    thread_property(DebugeeThreadId, system_thread_id(ThreadId)),
    dap_tracer_log(ThreadId),
    dap_tracer_state(S0),
    (   S0 = entry
    ->  Message = stopped(ThreadId, entry, "Program Started"),
        dap_tracer_next_state(stopped)
    ),
    thread_send_message(dap_server_debugee_queue, Message),
    dap_tracer_interrupt_server,
    dap_trace_interaction(ThreadId, Port, Frame, Choice, Action).

dap_tracer_interrupt_server :-
    put_code(dap_server_debugee_out, 3).

dap_trace_interaction(ThreadId, Port, Frame, Choice, Action) :-
    dap_tracer_log(dap_trace_interaction),
    dap_tracer_log(ThreadId),
    thread_get_message(Message),
    dap_tracer_log(Message),
    dap_tracer_handled_message(Message, ThreadId, Port, Frame, Choice, Action0),
    dap_tracer_log(Action0),
    (   Action0 = trace
    ->  dap_trace_interaction(ThreadId, Port, Frame, Choice, Action)
    ;   Action  = Action0
    ).

dap_tracer_handled_message(stack_trace(ThreadId), ThreadId, _Port, Frame, _Choice, trace) :-
    dap_tracer_log(dap_tracer_handled_message),
    dap_tracer_default_prolog_backtrace_depth(Depth),
    dap_tracer_log(Depth),
    get_prolog_backtrace(Depth, StackFrames0, [frame(Frame), guard(system:'<meta-call>'/1)]),
    dap_tracer_log(StackFrames0),
    dap_tracer_enriched_frames(StackFrames0, StackFrames),
    dap_tracer_log(ok),
    dap_tracer_log(StackFrames),
    thread_send_message(dap_server_debugee_queue, stack_trace(StackFrames)),
    dap_tracer_log(sent_message),
    dap_tracer_interrupt_server.

dap_tracer_default_prolog_backtrace_depth(16).

:- det(dap_tracer_enriched_frames/2).
dap_tracer_enriched_frames([H0|T0], [H|T]) :-
    dap_tracer_log(dap_tracer_enriched_frames),
    dap_tracer_log(H0),
    dap_tracer_enriched_frame(H0, H),
    dap_tracer_log(H),
    dap_tracer_log(dap_tracer_enriched_frames_loop),
    dap_tracer_enriched_frames(T0, T).

dap_tracer_enriched_frames([], []) :-
    dap_tracer_log(dap_tracer_enriched_frames_done).

:- det(dap_tracer_enriched_frame/2).
dap_tracer_enriched_frame(frame(Id, meta_call, _Goal), stack_frame(Id, meta, call)) :- !.
dap_tracer_enriched_frame(frame(Id, call(Module:F/A), _Goal), stack_frame(Id, Module, PI, File, Line, 0)) :- !,
    term_string(F/A, PI),
    dap_tracer_log(Id),
    dap_tracer_log(Module),
    dap_tracer_log(PI),
    (   predicate_property(Module:F, file(File))
    ->  true
    ;   File = null
    ),
    dap_tracer_log(File),
    (   predicate_property(Module:F, line_count(Line))
    ->  true
    ;   Line = 0
    ),
    dap_tracer_log(Line).
dap_tracer_enriched_frame(frame(Id, foreign(Module:PI0), _Goal), stack_frame(Id, Module, PI)) :- !,
    term_string(PI0, PI).
dap_tracer_enriched_frame(frame(Id, clause(ClauseRef, PC), _Goal), stack_frame(Id, Module, PI, File, Line, Column)) :-
    dap_tracer_log(clause),
    (   clause_info(ClauseRef, File, TPos, _), dap_tracer_log(haha)
    ->  dap_tracer_log(hahaa), '$clause_term_position'(ClauseRef, PC, List), dap_tracer_log(hahaaa),
        dap_tracer_log(List),
        dap_tracer_log(TPos),
        find_subgoal(List, TPos, PosTerm),
        dap_tracer_log(ahah),
        arg(1, PosTerm, StartOffset),
        dap_tracer_log(ahahh),
        file_offset_line_column(File, StartOffset, Line, Column),
        dap_tracer_log(ahahhh)
    ;   dap_tracer_log(oish), File = null, Line = 0, Column = 0
    ),
    dap_tracer_log(File),
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
