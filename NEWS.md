# What's new in SWI-Prolog Debug Adapter version 0.6.0

The following changes were introduced since version 0.5.0 of the
`debug_adapter` package:

## Added support for jumping to a choicepoint by force-failing the current goal

The `debug_adapter` server now interprets the `targetId` field of the
[`stepIn`](https://microsoft.github.io/debug-adapter-protocol/specification#Requests_StepIn)
DAP request to determine from where the execution of the debuggee
should be commenced, i.e. whether to perform a regular "step in" along
the programs execution or to alter the state of the debuggee such that
it continues to run from some other point.

To obtain the set of possible "targets" the debuggee can step into at
a certain time, clients issue a
[`stepInTargets`](https://microsoft.github.io/debug-adapter-protocol/specification#Requests_StepInTargets)
DAP request, to which the `debug_adapter` responds with an array of
possible targets. In deterministic contexts the server reports only
one possible target which corresponds to a regular "step in" down the
execution path, while in presence of choicepoints the server reports
with another target that can be specified in order to force-fail the
paused goal and continue execution in next choicepoint.

An appropriate end-to-end test of requesting `stepInTargets` before
and after the debuggee executes a cut (`!/0`), transitioning from a
nondeterministic context to a deterministic one, was added to the test
suite.


## Added support for exception breakpoints

Exception breakpoints are a standard DAP feature defined in [the
specification](https://microsoft.github.io/debug-adapter-protocol/specification),
which controls if and when the debuggee will be stopped upon throwing
an exception.
During session startup the server reports which
"filters" it supports for trapping exceptions. Each filter defines a
server-defined set of exception terms that the server will trap in the
current session.

The client can then use the
[`setExceptionBreakpoints`](https://microsoft.github.io/debug-adapter-protocol/specification#Requests_SetExceptionBreakpoints)
DAP request to apply or remove these filters in order to specify which
exceptions are propagated and which are trapped. Initially the server
does not trap any exceptions unless explicitly requested, and sending
a set of exception breakpoint filters to the server first clears any
existing exception breakpoints, hence a `setExceptionBreakpoints`
request specifying an empty filter array has a similar effect as the
query:
```prolog
?- notrap(_).
```

An end-to-end test of setting and hitting exception breakpoints was
added to the test suite.


The `debug_adapter` server currently supports only one filter, with the ID
"true" and the description "Trap exceptions", which corresponds to trapping
all thrown exceptions, i.e. has a similar effect as:
```prolog
?- gtrap(_).
```

![trap](gallery/trap.png)

## Added support for function breakpoints

Function breakpoints are a standard DAP feature defined in [the
specification](https://microsoft.github.io/debug-adapter-protocol/specification).
Function breakpoints allow the user to define runtime locations in
which the debuggee shall break in terms of specific runtime procedures
("functions", or in our case predicates), in contrast to plain
(source-)breakpoints which define break locations in terms of the
correlating source code.

Function breakpoints can now be set via the
[setFunctionBreakpoints](https://microsoft.github.io/debug-adapter-protocol/specification#Requests_SetFunctionBreakpoints)
standard DAP request specifying an array of Prolog predicate
indicators (e.g. `foo/2`). The `debug_adapter` server attempts to
install a spy point on each specified predicate using [`spy/1`](https://www.swi-prolog.org/pldoc/man?predicate=spy/1) and
responds with a array of DAP
[Breakpoint](https://microsoft.github.io/debug-adapter-protocol/specification#Types_Breakpoint)s
denoting for each predicate whether spying succeeded or failed via the
`verified` field of the corresponding element in the array.

Note that a new `setFunctionBreakpoints` request instructs the server
to remove all exiting function breakpoints before installing the new
ones, so for instance an empty array can be used to clear all function
breakpoints in a given debug session.

At runtime, if the debuggee calls a predicate that is being spied on,
the thread that hit the installed spy point is stopped and the
`debug_adapter` server reports an appropriate DAP
[`stopped`](https://microsoft.github.io/debug-adapter-protocol/specification#Events_Stopped)
event with the `reason` field set to "function breakpoint".

An end-to-end test of setting and hitting function breakpoints was
added to the test suite.


## Report which breakpoint was hit when stopping on breakpoints

The `debug_adapter` server will now set the `hitBreakpointIds` field of
[`stopped`](https://microsoft.github.io/debug-adapter-protocol/specification#Events_Stopped)
DAP event, specifying the breakpoints that were actually hit causing
the debuggee to stop whenever a breakpoint is hit.
Clients can use this information to visually distinguish the hit breakpoints.


## Report debuggee initial goal results and bindings when no top-level is attached

The `debug_adapter` server will now report the result and the
variables bindings of the goal specified during session startup in the
[`launch`](https://microsoft.github.io/debug-adapter-protocol/specification#Requests_Launch)
request via DAP
[`output`](https://microsoft.github.io/debug-adapter-protocol/specification#Events_Output)
events. This makes it simpler to examine or save the results of executed
goals after the debug session is over.
