# Debug Adapter Protocol for SWI-Prolog
This repository hosts the development of [DAP](https://microsoft.github.io/debug-adapter-protocol/) for [SWI-Prolog](https://www.swi-prolog.org/), written in Prolog.

## Installation
Clone and build the server:
```sh
$ git clone https://github.com/eshelyaron/debug_adapter.git
$ cd debug_adapter
$ ./configure
$ make
```

`make` will build the Debug Adapter server at `prolog/swi_debug_adapter`. Optionally, run the test-suite and install the server to make it accessible from your $PATH:
```sh
$ make check
$ make install
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
- [x] Restarting debuged frames
- [ ] Supporting more debug requests - `stepOut`, `stepBack`, `next`
- [x] Providing source code for dynamic predicates through decompilation
- [ ] Installing breakpoints at source positions
- [ ] Providing information about variable bindings in each frame
- [ ] Editing and reloading source code during break
- [ ] ...

## Demo
![stepIn Demo](stepIn.gif)
