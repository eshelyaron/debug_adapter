# Debug Adapter Protocol for SWI-Prolog
This repository hosts the development of [DAP](https://microsoft.github.io/debug-adapter-protocol/) for [SWI-Prolog](https://www.swi-prolog.org/), written in Prolog.

## Installation
Make sure you have SWI-Prolog (the `swipl` executable) installed, and run the following commands in the root of this repository to build the DAP server at `bin/debug_adapter`:
```sh
$ make
$ make check
```

If you're using GNU Emacs, load `elisp/dap-swi-prolog.el` and add the following lines to your `init.el`:
```elisp
(require 'dap-swi-prolog)
```

Now run `M-x dap-debug` in a Prolog buffer and have fun.

## Status
Work in progress, see [Roadmap](#roadmap)

## Roadmap
The two big milestones for this project are:
* Reaching feature parity with SWI-Prolog's [built-in graphical debugger](https://www.swi-prolog.org/pldoc/man?section=guitracer) with a DAP enabled IDE (GNU Emacs + `dap-mode` in particular), and
* Supporting the entire DAP specification

Some of the needed steps towards these goals are:

- [x] Stepping through the source code of a debugee with precise source positions
- [ ] Stepping over, backwards, etc.
- [ ] Providing source code for dynamic predicates through decompilation
- [ ] Installing breakpoints at source positions
- [ ] Providing information about variable bindings in each frame
- [ ] Editing and reloading source code during break
- [ ] ...

## Demo
![stepIn Demo](stepIn.gif)
