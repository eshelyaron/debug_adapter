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

| Event          | Description                                                 | Status |
|----------------|-------------------------------------------------------------|--------|
| Breakpoint     | information about a breakpoint has changed                  |        |
| Capabilities   | one or more capabilities have changed                       |        |
| Continued      | the execution of the debuggee has continued                 | ✅     |
| Exited         | the debuggee has exited and returns its exit code           | ✅     |
| Initialized    | the debug adapter is ready to accept configuration requests | ✅     |
| Invalidated    | some state in the debug adapter has changed                 |        |
| LoadedSource   | source has been added, changed, or removed                  | ✅     |
| Memory         | memory range has been updated                               |        |
| Module         | information about a module has changed                      |        |
| Output         | the target has produced some output                         | ✅     |
| Process        | the debugger has begun debugging a new process              |        |
| ProgressEnd    | signals the end of the progress reporting                   |        |
| ProgressStart  | a long running operation is about to start                  |        |
| ProgressUpdate | progress reporting needs to updated                         |        |
| Stopped        | the execution of the debuggee has stopped                   | ✅     |
| Terminated     | debugging of the debuggee has terminated                    |        |
| Thread         | a thread has started or exited                              | ✅     |


### DAP Requests implementation coverage
`request` messages are sent from the client to the server to retrieve
information about the current runtime.

| Request                   | Description                                                          | Status |
|---------------------------|----------------------------------------------------------------------|--------|
| Attach                    | attach to a debuggee that is already running                         | ✅     |
| BreakpointLocations       | obtain possible locations for source breakpoints in a given range    |        |
| Completions               | obtain possible completions for a given caret position and text      |        |
| ConfigurationDone         | finish initialization of the debug adapter                           | ✅     |
| Continue                  | resume execution of all threads                                      | ✅     |
| DataBreakpointInfo        | obtain information on a possible data breakpoint that could be set   |        |
| Disassemble               | disassemble code at the provided location                            |        |
| Disconnect                | stop debugging                                                       | ✅     |
| Evaluate                  | evaluate the given expression in the context of the debuggee         | ✅     |
| ExceptionInfo             | obtain details about thrown exceptions                               | ✅     |
| Goto                      | set the location where the debuggee will continue to run             |        |
| GotoTargets               | obtain possible goto targets for the specified source location       |        |
| Initialize                | configure the debug adapter and obtain its capabilities              | ✅     |
| Launch                    | start the debuggee                                                   | ✅     |
| LoadedSources             | obtain all sources currently loaded by the debuggee                  |        |
| Modules                   | obtain modules currently loaded by the debuggee                      |        |
| Next                      | execute the specified thread for one step                            | ✅     |
| Pause                     | suspend the debuggee                                                 | ✅     |
| ReadMemory                | read bytes from memory at the specified location                     |        |
| Restart                   | restart the debug session                                            |        |
| RestartFrame              | restart execution of the specified stackframe                        | ✅     |
| ReverseContinue           | resume backward execution of all threads                             |        |
| Scopes                    | obtain the variable scopes for a given stackframe                    | ✅     |
| SetBreakpoints            | set breakpoints in specified source locations                        | ✅     |
| SetDataBreakpoints        | set data breakpoints                                                 |        |
| SetExceptionBreakpoints   | set the debug adapter's behaviour upon debuggee exceptions           | ✅     |
| SetExpression             | assign runtime values to a given expression                          |        |
| SetFunctionBreakpoints    | set functions breakpoints                                            | ✅     |
| SetInstructionBreakpoints | set intructions breakpoints                                          |        |
| SetVariable               | assign runtime values to a given variable                            |        |
| Source                    | obtain source code for a given source reference                      | ✅     |
| StackTrace                | obtain stacktrace from the current execution state of a given thread | ✅     |
| StepBack                  | execute the specified thread one backward step                       |        |
| StepIn                    | resume the specified thread to step into a function                  | ✅     |
| StepInTargets             | obtain possible stepIn targets for the specified stackframe          |        |
| StepOut                   | resume the specified thread to step out from a function              | ✅     |
| Terminate                 | terminate the specified thread                                       |        |
| TerminateThreads          | terminate the specified threads                                      |        |
| Threads                   | obtain the list of current threads                                   | ✅     |
| Variables                 | obtain all child variables for the specified variable reference      | ✅     |
| WriteMemory               | write bytes to memory at the specified location                      |        |


### DAP Reverse requests implementation coverage
Reverse requests are `request` messages sent from the server to the
client to trigger client side operations.

| Reverse Request | Description                                   | Status |
|-----------------|-----------------------------------------------|--------|
| RunInTerminal   | run a command in a client controlled terminal | ✅     |


## Roadmap
The two big milestones for this project are:
* Reaching feature parity with SWI-Prolog's [built-in graphical debugger](https://www.swi-prolog.org/pldoc/man?section=guitracer) with a DAP enabled IDE (GNU Emacs + `dap-mode` in particular), and
* Supporting the entire DAP specification

Some of the needed steps towards these goals are:

- [x] Stepping through the source code of a debuggee with precise source positions
- [x] Restarting debuged frames
- [x] Supporting more debug requests - `stepOut`, `stepBack`, `next`
- [x] Providing source code for dynamic predicates through decompilation
- [x] Installing breakpoints at source positions
- [x] Providing information about variable bindings in each frame
- [x] Editing and reloading source code during break
- [ ] ...
