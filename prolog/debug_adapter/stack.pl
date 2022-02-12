:- module(
       da_stack,
       [
           da_stack_trace/2,
           da_stack_frame_at_port/4
       ]
   ).

:- use_module(frame).

/** <module> DAP library module for reasoning about Prolog execution stack

This module contains predicates for retrieving information about the current Prolog _stack_ for
debugging purposes.

*/


%!  da_stack_trace(+FrameId, -StackTrace) is det.
%
%   StackTrace is unified with a list of describing the current execution stack, starting from
%   FrameId.  The stack frame corresponding to FrameId itself is not included in StackTrace.
%
%   Each element of StackTrace is a compound term of the form
%   `stack_frame(StackFrameId, PredicateIndicator, Alternative, SourceSpan)`, where:
%     - StackFrameId is the temporarly unique ID of the frame,
%     - PredicateIndicator is the qualified predicate indicator of the frame's goal,
%     - Alternative is a term describing the location from which execution will be resumed in case
%       the frame's goal fails and
%     - SourceSpan is the the portion of source code which defines the call to the frame's goal
%       within the enclosing clause, also known as the _call site_ of the frame.

:- det(da_stack_trace/2).
da_stack_trace(FrameId, StackTrace) :-
    da_frame_parent(FrameId, ParentFrameId),
    da_frame_parent_pc(FrameId, PC),
    da_frame_pc_stack(ParentFrameId, PC, da_stack_frame_info, StackTrace).

da_stack_frame_info(FrameId, PC,
                    stack_frame(FrameId, pc(PC), PredicateIndicator, label(frame, AlternativeFrameId), SourceSpan)) :-
    da_frame_predicate_indicator(FrameId, PredicateIndicator),
    da_frame_alternative_frame(FrameId, AlternativeFrameId),
    da_frame_pc_source_span(FrameId, PC, SourceSpan).

da_stack_frame_at_port(FrameId, Port, ChoicePoint,
                       stack_frame(FrameId, port(Port), PredicateIndicator, Alternative, SourceSpan)) :-
    da_frame_predicate_indicator(FrameId, PredicateIndicator),
    da_frame_alternative(FrameId, ChoicePoint, Alternative),
    da_frame_port_source_span(FrameId, Port, SourceSpan).
