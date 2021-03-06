:- module(
       da_frame,
       [
           da_frame_stack/3,
           da_hidden_frame/1,
           da_frame_parent/2,
           da_frame_clause/2,
           da_frame_predicate_indicator/2,
           da_alternative/2,
           da_frame_alternative_frame/2,
           da_frame_parent_pc/2,
           da_frame_pc_stack/4,
           da_frame_pc_source_span/3,
           da_frame_clause_source_span/2,
           da_frame_port_source_span/3,
           da_frame_scopes/4,
           da_frame_evaluate/4,
           da_frame_variables_mapping/2,
           da_referenced_variables/2,
           da_frame_step_in_targets/4
       ]
   ).

:- use_module(source).
:- use_module(clause).


/** <module> DAP library module for reasoning about Prolog frames

This module contains predicates for retrieving information about Prolog _frames_ for debugging
purposes.  A Prolog _frame_ is a runtime artifact that encapsulates the execution of a single
goal.

Each frame has a unique temporarly ID represented as an integer. The predicates in this module
generally take a frame ID as their first argument and unify their second argument with information
about the denoted frame which is relevant for debugging purposes.

@see prolog_frame_attribute/3
@see da_frame_stack/2
*/


:- meta_predicate da_frame_stack(+, 3, -).
:- meta_predicate da_frame_pc_stack(+, +, 3, -).

%!  da_frame_stack(+FrameId, :Goal, -Frames) is det.
%
%   Frames is unified with a list of StackFrame terms acquired by meta-calling Goal as
%   `call(Goal,Id, PC, StackFrame)` for every non-hidden frame in the current execution
%   stack starting after FrameId, with the frame's ID and saved PC as the first and
%   second argument to Goal respectively.
%
%   @see da_frame_parent/2
%   @see da_frame_parent_pc/2

:- det(da_frame_stack/3).
da_frame_stack(Frame, Goal, Frames) :-
    da_frame_parent(Frame, Parent),
    da_frame_parent_pc(Frame, PC),
    da_frame_pc_stack(Parent, PC, Goal, Frames).

da_frame_pc_stack(null, _, _, []) :- !.
da_frame_pc_stack(FrameId, _, Goal, Frames) :-
    da_hidden_frame(FrameId),
    !,
    da_frame_stack(FrameId, Goal, Frames).
da_frame_pc_stack(Frame, PC, Goal, [Head|Frames]) :-
    call(Goal, Frame, PC, Head),
    da_frame_stack(Frame, Goal, Frames).


%!  da_frame_parent(+Frame, -Parent) is det.
%
%   Parent is unified with the parent frame of Frame, or with the atom `null` in case Frame is the
%   top frame.

:- det(da_frame_parent/2).
da_frame_parent(Frame, Parent) :-
    prolog_frame_attribute(Frame, parent, Parent),
    !.
da_frame_parent(_, null).


:- det(da_frame_non_hidden_parent/2).
da_frame_non_hidden_parent(null, null) :- !.
da_frame_non_hidden_parent(FrameId, ParentFrameId) :-
    da_frame_parent(FrameId, ParentFrameId0),
    da_frame_parent_non_hidden_parent(ParentFrameId0, ParentFrameId).

da_frame_parent_non_hidden_parent(null, null) :- !.
da_frame_parent_non_hidden_parent(ParentFrameId0, ParentFrameId) :-
    da_hidden_frame(ParentFrameId0),
    !,
    da_frame_non_hidden_parent(ParentFrameId0, ParentFrameId).
da_frame_parent_non_hidden_parent(ParentFrameId, ParentFrameId).


%!  da_frame_parent_pc(+Frame, -PC) is det.
%
%   PC is unified with the program counter saved by Frame on behalf of the parent frame of Frame, or
%   with the atom `null` in case Frame does not specify a saved program counter.
%
%   The saved program counter determines from which point in the *parent* frame execution will
%   resume once Frame is finished.
%
%   @see da_frame_parent/2
%   @see prolog_frame_attribute/3 section on the `pc` option for information
%   regarding for which frames the saved program counter is not available.

:- det(da_frame_parent_pc/2).
da_frame_parent_pc(Frame, PC) :-
    prolog_frame_attribute(Frame, pc, PC),
    da_frame_parent(Frame, Parent),
    \+ da_hidden_frame(Parent),
    !.
da_frame_parent_pc(_, null).


%!  da_hidden_frame(+Frame) is semidet.
%   True when Frame ought to be hidden during debugging.

da_hidden_frame(Frame) :-
    prolog_frame_attribute(Frame, hidden, true), !.
da_hidden_frame(Frame) :-
    prolog_frame_attribute(Frame, goal, Goal),
    da_hidden_predicate(Goal), !.

da_hidden_predicate(Goal) :- predicate_property(Goal, nodebug), !.
da_hidden_predicate(Goal) :- predicate_property(Goal, hidden), !.
da_hidden_predicate(swipl_debug_adapter:_).


%!  da_frame_predicate_indicator(+FrameId, -PredicateIndicator) is det.
%
%   PredicateIndicator is unified with the qualified predicate indicator of the goal executed in
%   frame FrameId.

:- det(da_frame_predicate_indicator/2).
da_frame_predicate_indicator(FrameId, PredicateIndicator) :-
    prolog_frame_attribute(FrameId, predicate_indicator, PredicateIndicator).


%!  da_alternative(+ChoicePoint, -Alternative) is det.
%
%   Alternative is unified with a term describing the location from which execution will be
%   resumed in case the current goal fails, which is one of the following:
%    - frame(AlternativeFrameId)
%      If ChoicePoint refers to an _alternative frame_, where AlternativeFrameId is the ID of the
%      frame from which execution will resume in case the goal associated with the current frame fails,
%    - jump(PC)
%      If ChoicePoint is an in-clause choice point, where PC is the program counter in the frame
%      from which execution will resume is case the current goal fails or
%    - clause(Clause)
%      If ChoicePoint refers to an alternative clause Clause
%    - null
%      If ChoicePoint is `none`

:- det(da_alternative/2).
da_alternative(ChoicePoint, Alternative) :-
    prolog_choice_attribute(ChoicePoint, type, Type),
    da_alternative(Type, ChoicePoint, Alternative).

:- det(da_alternative/3).
da_alternative(jump, ChoicePoint, jump(PC)) :-
    !,
    prolog_choice_attribute(ChoicePoint, pc, PC).
da_alternative(clause, ChoicePoint, clause(Clause)) :-
    !,
    prolog_choice_attribute(ChoicePoint, clause, Clause).
da_alternative(none, _, null) :- !.
da_alternative(debug, _, null) :- !.
da_alternative(catch, _, null) :- !.
da_alternative(top, _, null) :- !.
da_alternative(_, ChoicePoint, frame(Frame)) :-
    !,
    prolog_choice_attribute(ChoicePoint, frame, Frame).


%!  da_frame_alternative_frame(+FrameId, -AlternativeFrameId) is det.
%
%   AlternativeFrameId is unified with the ID of the frame that will be tried if FrameId fails, or
%   with the atom `null` if FrameId does not have an alternative frame.

:- det(da_frame_alternative_frame/2).
da_frame_alternative_frame(FrameId, frame(AlternativeFrameId)) :-
    prolog_frame_attribute(FrameId, alternative, AlternativeFrameId),
    !.
da_frame_alternative_frame(_, null).


%!  da_frame_clause(+Frame, -ClauseRef) is det.
%
%   ClauseRef is unified with a reference to the clause which Frame is executing, or with the atom
%   `null` in case Frame is executing a foreign predicate.

:- det(da_frame_clause/2).
da_frame_clause(Frame, ClauseRef) :-
    (   prolog_frame_attribute(Frame, clause, ClauseRef)
    ->  true
    ;   prolog_frame_attribute(Frame, goal, Goal),
        qualified(Goal, Module, UGoal),
        (   predicate_property(Module:UGoal, interpreted)
        ->  (   clause(Module:UGoal, _Body, ClauseRef)
            ->  true
            ;   functor(UGoal, Functor, Arity),
                functor(UGoalTemplate, Functor, Arity),
                clause(Module:UGoalTemplate, _Body, ClauseRef), !
            )
        ;   ClauseRef = null
        )
    ).

:- det(da_frame_pc_source_span/3).
da_frame_pc_source_span(_, null, null) :- !.
da_frame_pc_source_span(FrameId, PC, SourceSpan) :-
    da_frame_clause(FrameId, ClauseRef),
    da_clause_source_span(ClauseRef, SourceSpan, [pc(PC)]).

:- det(da_frame_port_source_span/3).
da_frame_port_source_span(FrameId, Port, SourceSpan) :-
    da_port_parent_pc(Port, PC),
    !,
    da_frame_parent_port_source_span(FrameId, FrameId, PC, Port, SourceSpan).
da_frame_port_source_span(FrameId, Port, SourceSpan) :-
    da_frame_parent_pc(FrameId, ParentPC),
    da_frame_parent_pc_source_span(FrameId, ParentPC, Port, SourceSpan).

da_port_parent_pc(cut_call(PC), PC) :- !.
da_port_parent_pc(cut_exit(PC), PC) :- !.
da_port_parent_pc(redo(0)     , _ ) :- !, false.
da_port_parent_pc(redo(PC)    , PC) :- !.

da_frame_parent_pc_source_span(FrameId, null, Port, SourceSpan) :-
    !,
    da_frame_port_clause_source_span(FrameId, Port, SourceSpan).
da_frame_parent_pc_source_span(FrameId, ParentPC, Port, SourceSpan) :-
    da_frame_parent(FrameId, ParentFrameId),
    da_frame_parent_port_source_span(FrameId, ParentFrameId, ParentPC, Port, SourceSpan).

da_frame_port_clause_source_span(FrameId, Port, SourceSpan) :-
    da_frame_clause(FrameId, ClauseRef),
    da_clause_source_span(ClauseRef, SourceSpan, [port(Port)]).

da_frame_clause_source_span(FrameId, SourceSpan) :-
    da_frame_clause(FrameId, ClauseRef),
    da_clause_source_span(ClauseRef, SourceSpan).

:- det(da_frame_parent_port_source_span/5).
da_frame_parent_port_source_span(FrameId, null, _ParentPC, Port, SourceSpan) :-
    !,
    da_frame_port_clause_source_span(FrameId, Port, SourceSpan).
da_frame_parent_port_source_span(FrameId, _ParentFrameId, _ParentPC, unify, SourceSpan) :-
    !,
    da_frame_clause(FrameId, ClauseRef),
    da_clause_source_span(ClauseRef, SourceSpan, [port(unify)]).
da_frame_parent_port_source_span(_FrameId, ParentFrameId, ParentPC, Port, SourceSpan) :-
    da_frame_clause(ParentFrameId, ParentClauseRef),
    da_clause_source_span(ParentClauseRef, SourceSpan, [pc(ParentPC), port(Port)]).


:- det(da_frame_scopes/4).
da_frame_scopes(ActiveFrameId, ActiveFrameId, Port, Scopes) :-
    !,
    da_active_frame_scopes(ActiveFrameId, Port, Scopes).
da_frame_scopes(FrameId, _ActiveFrameId, _Port, Scopes) :-
    da_stack_frame_scopes(FrameId, Scopes).

da_stack_frame_scopes(FrameId, [ scope(Name, ArgumentsRef, SourceSpan),
                                 scope("Local Bindings", LocalsRef, SourceSpan)
                               ]
                     ) :-
    da_frame_variables_reference_type(FrameId, ArgumentsRef, arguments),
    da_frame_variables_reference_type(FrameId, LocalsRef, locals),
    da_frame_clause_source_span(FrameId, SourceSpan),
    da_frame_predicate_indicator(FrameId, PI),
    format(string(Name), "~w Arguments", PI).

da_active_frame_scopes(FrameId, unify, Scopes) :-
    !,
    da_stack_frame_scopes(FrameId, Scopes).
da_active_frame_scopes(FrameId, Port, [ scope(Name, ArgumentsRef, ArgumentsSpan),
					scope("Local Bindings", LocalsRef, LocalsSpan)
                                      ]
                      ) :-
    !,
    da_frame_non_hidden_parent(FrameId, ParentFrameId),
    (   ParentFrameId == null
    ->  da_frame_variables_reference_type(FrameId, LocalsRef, locals),
        da_frame_port_source_span(FrameId, Port, LocalsSpan)
    ;   da_frame_variables_reference_type(ParentFrameId, LocalsRef, locals),
        da_frame_clause_source_span(ParentFrameId, LocalsSpan)
    ),
    da_frame_variables_reference_type(FrameId, ArgumentsRef, arguments),
    da_frame_port_source_span(FrameId, Port, ArgumentsSpan),
    da_frame_predicate_indicator(FrameId, PI),
    format(string(Name), "~w Arguments", PI).


:- use_module(library(clpfd)).

:- det(da_frame_variables_reference_type/3).
da_frame_variables_reference_type(FrameId, VariablesRef, Type) :-
    da_variables_reference_frame_type_id(VariablesRef, FrameId, TypeId),
    da_scope_type_id(Type, TypeId).

da_variables_reference_frame_type_id(VariablesRef, FrameId, TypeId) :-
    TypeId in 0..2,
    VariablesRef #= (FrameId * 4) + TypeId.

da_scope_type_id(arguments, 0) :- !.
da_scope_type_id(locals, 1) :- !.
da_scope_type_id(cached, 2).

:- det(da_referenced_variables/2).
da_referenced_variables(VariablesRef, Variables) :-
    da_frame_variables_reference_type(FrameId, VariablesRef, Type),
    (   Type == cached
    ->  da_variables_compound_arguments(Variables0, FrameId),
        indexed_arguments(1, Variables0, Variables)
    ;   da_frame_clause(FrameId, ClauseRef),
        (   ClauseRef == null
        ->  da_frame_goal_arity(FrameId, Arity),
            findall('_', between(1, Arity, _), Args),
            compound_name_arguments(VarNames, varnames, Args)
        ;   da_clause_variable_names(ClauseRef, VarNames)
        ),
        da_frame_variables(FrameId, Type, VarNames, Variables)
    ).

:- thread_local da_variables_compound_arguments/2.

da_tracer_cached_compound_arguments(Arguments, Ref) :-
    (   da_variables_compound_arguments(_, Ref0)
    ->  succ(Ref0, Ref)
    ;   Ref = 1
    ),
    asserta(da_variables_compound_arguments(Arguments, Ref)).

:- det(indexed_arguments/3).
indexed_arguments(_, [], []) :- !.
indexed_arguments(I0, [H0|T0], [variable(Name, H, ChildrenReference)|T]) :-
    indexed_argument_name(I0, Name),
    da_term_factorized(H0, ChildrenReference, H),
    succ(I0, I1),
    indexed_arguments(I1, T0, T).

indexed_argument_name(I, N) :-
    format(atom(N), "arg(~w)", [I]).

da_term_factorized(Var, 0, Name) :-
    var(Var), format(string(Name), "~w", [Var]),
    !.
da_term_factorized([], 0, "[]") :- !.
da_term_factorized(List, Ref, "[...]") :-
    is_list(List),
    !,
    da_tracer_cached_compound_arguments(List, Ref0),
    Ref is (Ref0 << 2) + 2.
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

:- det(da_frame_variables/4).
da_frame_variables(Frame, Type, VarNames, Variables) :-
    da_frame_goal_arity(Frame, Arity),
    da_frame_arity_variables(Frame, Arity, Type, VarNames, Variables).

:- det(da_frame_goal_arity/2).
da_frame_goal_arity(FrameId, Arity) :-
    prolog_frame_attribute(FrameId, predicate_indicator, PI),
    qualified(PI, _, _Functor/Arity).

da_frame_arity_variables(_, _, _, varnames, []) :-
    % clause has no variables at all, hence VarNames is an atom
    !.
da_frame_arity_variables(_, 0, arguments, _, []) :- !.
da_frame_arity_variables(Frame, Arity, arguments, VarNames, Variables) :-
    !,
    da_frame_arguments(Frame, 1, Arity, VarNames, Variables).
da_frame_arity_variables(Frame, Arity, locals, VarNames, Variables) :-
    !,
    succ(Arity, I),
    da_frame_locals(Frame, I, VarNames, Variables).

:- det(da_frame_arguments/5).
da_frame_arguments(Frame, I, Arity, VarNames, Variables) :-
    (   I =< Arity
    ->  arg(I, VarNames, Name0),
        (   Name0 == '_'
        ->  indexed_argument_name(I, Name)
        ;   Name = Name0
        ),
        prolog_frame_attribute(Frame, argument(I), Value0),
        da_term_factorized(Value0, ChildrenReference, Value),
        Variables = [variable(Name, Value, ChildrenReference)|T],
        NI is I + 1,
        da_frame_arguments(Frame, NI, Arity, VarNames, T)
    ;   Variables = []
    ).

da_frame_locals(Frame, I, VarNames, Variables) :-
    (   arg(I, VarNames, Name0)
    ->  (   Name0 == '_'
        ->  indexed_argument_name(I, Name)
        ;   Name = Name0
        ),
        (   prolog_frame_attribute(Frame, argument(I), Value0)
        ->  da_term_factorized(Value0, ChildrenReference, Value)
        ;   da_term_factorized(_, ChildrenReference, Value)
        ),
        Variables = [variable(Name, Value, ChildrenReference)|T],
        NI is I + 1,
        da_frame_locals(Frame, NI, VarNames, T)
    ;   Variables = []
    ).


:- det(da_frame_evaluate/4).
da_frame_evaluate(FrameId, SourceTerm, Result, Bindings) :-
    read_term_from_atom(SourceTerm, Goal, [variable_names(Bindings)]),
    da_frame_clause(FrameId, ClauseRef),
    da_clause_variable_names(ClauseRef, ClauseVarNames),
    da_frame_unify_variables(FrameId, ClauseVarNames, Bindings),
    da_evaluate(Goal, Result).


:- det(da_evaluate/2).
da_evaluate(Goal, Result) :-
    catch(Goal, Result, true),
    !,
    (   var(Result)
    ->  Result = true
    ;   true
    ).
da_evaluate(_, false).


:- det(da_frame_unify_variables/3).
da_frame_unify_variables(_FrameId, _ClauseVarNames, [               ]) :- !.
da_frame_unify_variables( FrameId,  ClauseVarNames, [VarName=Value|T]) :- !,
    (   arg(I, ClauseVarNames, VarName)
    ->  prolog_frame_attribute(FrameId, argument(I), Value),
        da_frame_unify_variables(FrameId, ClauseVarNames, T)
    ;   true
    ),
    da_frame_unify_variables(FrameId, ClauseVarNames, T).


da_frame_variables_mapping(FrameId, Map) :-
    da_frame_clause(FrameId, ClauseRef),
    da_clause_variable_names(ClauseRef, ClauseVarNames),
    (   ClauseVarNames == varnames
    ->  Map = []
    ;   findall(Name=Value,
                (   arg(I, ClauseVarNames, Name),
                    prolog_frame_attribute(FrameId, argument(I), Value)
                ),
                Map)
    ).


da_frame_step_in_targets(FrameId, FrameId, Choice, [step_in_target(0, null)|Targets]) :-
    !,
    da_alternative(Choice, Alternative),
    da_alternative_step_in_targets(Alternative, Targets).
da_frame_step_in_targets(_, _, _, []).


da_alternative_step_in_targets(null, []) :- !.
da_alternative_step_in_targets(Alt, [step_in_target(1, Alt)]).
