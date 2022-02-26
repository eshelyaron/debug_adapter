# Debug Adapter Protocol for SWI-Prolog

Welcome to [the SWI-Prolog Debug Adapter Protocol server](https://github.com/eshelyaron/debug_adapter),
an implementation of [DAP](https://microsoft.github.io/debug-adapter-protocol/) for
[SWI-Prolog](https://www.swi-prolog.org/), written in Prolog.

## Installation

`debug_adapter` is available as a SWI-Prolog [pack](https://www.swi-prolog.org/pack/list?p=debug_adapter),
to use it simply run the following goal in the SWI-Prolog toplevel:
```prolog
?- pack_install(debug_adapter).
```

If you're using GNU Emacs with `dap-mode`, add the following lines to your `init.el`:
```elisp
(require 'dap-swi-prolog)
```

Now run `M-x dap-debug` in a Prolog buffer and have fun.

## Status

Most of the DAP specification is implemented and [operational](https://github.com/eshelyaron/debug_adapter/blob/main/gallery.md).
For the status of specific features, see below.

Currently only tested with GNU Emacs and `dap-mode` until we have a VS Code extension.


### DAP Events implementation coverage
`event` messages are notifications sent from the server to the client according to different
[runtime hooks](https://www.swi-prolog.org/pldoc/man?section=hooks) that are implemented by the
server.

| Event          | Status |
|----------------|--------|
| Breakpoint     |        |
| Capabilities   |        |
| Continued      | ✅      |
| Exited         | ✅      |
| Initialized    | ✅      |
| Invalidated    |        |
| LoadedSource   | ✅      |
| Memory         |        |
| Module         |        |
| Output         | ✅      |
| Process        |        |
| ProgressEnd    |        |
| ProgressStart  |        |
| ProgressUpdate |        |
| Stopped        | ✅      |
| Terminated     |        |
| Thread         | ✅      |


### DAP Requests implementation coverage
`request` messages are sent from the client to the server to retrieve
information about the current runtime.

| Request                   | Status |
|---------------------------|--------|
| Attach                    | ✅      |
| BreakpointLocations       |        |
| Completions               |        |
| ConfigurationDone         | ✅      |
| Continue                  | ✅      |
| DataBreakpointInfo        |        |
| Disassemble               |        |
| Disconnect                | ✅      |
| Evaluate                  | ✅      |
| ExceptionInfo             | ✅      |
| Goto                      |        |
| GotoTargets               |        |
| Initialize                | ✅      |
| Launch                    | ✅      |
| LoadedSources             |        |
| Modules                   |        |
| Next                      | ✅      |
| Pause                     | ✅      |
| ReadMemory                |        |
| Restart                   |        |
| RestartFrame              | ✅      |
| ReverseContinue           |        |
| Scopes                    | ✅      |
| SetBreakpoints            | ✅      |
| SetDataBreakpoints        |        |
| SetExceptionBreakpoints   |        |
| SetExpression             |        |
| SetFunctionBreakpoints    |        |
| SetInstructionBreakpoints |        |
| SetVariable               |        |
| Source                    | ✅      |
| StackTrace                | ✅      |
| StepBack                  |        |
| StepIn                    | ✅      |
| StepInTargets             |        |
| StepOut                   | ✅      |
| Terminate                 |        |
| TerminateThreads          |        |
| Threads                   | ✅      |
| Variables                 | ✅      |
| WriteMemory               |        |


### DAP Reverse requests implementation coverage
Reverse requests are `request` messages sent from the server to the
client to trigger client side operations.

| Reverse Request | Status |
|-----------------|--------|
| RunInTerminal   | ✅      |


## Roadmap
The two big milestones for this project are:
* Reaching feature parity with SWI-Prolog's [built-in graphical debugger](https://www.swi-prolog.org/pldoc/man?section=guitracer) with a DAP enabled IDE (GNU Emacs + `dap-mode` in particular), and
* Supporting the entire DAP specification

Some of the needed steps towards these goals are:

- [x] Stepping through the source code of a debugee with precise source positions
- [x] Restarting debuged frames
- [x] Supporting more debug requests - `stepOut`, `stepBack`, `next`
- [x] Providing source code for dynamic predicates through decompilation
- [x] Installing breakpoints at source positions
- [x] Providing information about variable bindings in each frame
- [x] Editing and reloading source code during break
- [ ] ...
