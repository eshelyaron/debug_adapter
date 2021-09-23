# Debug Adapter Protocol for SWI-Prolog
This repository hosts the development of [DAP](https://microsoft.github.io/debug-adapter-protocol/) for [SWI-Prolog](https://www.swi-prolog.org/), written in Prolog.

## Installation
Make sure you have SWI-Prolog (the `swipl` executable) installed, and run:
```sh
$ make
$ make install
```

## Status
Work in progress, see [Roadmap](#roadmap)

## Roadmap
The two big milestones for this project are:
* Reaching feature parity with SWI-Prolog's [built-in graphical debugger](https://www.swi-prolog.org/pldoc/man?section=guitracer) with a DAP enabled IDE (GNU Emacs + `dap-mode` in particular), and
* Supporting the entire DAP specification

Some of the needed steps to get to these goals are:

- [x] Supporting stepping through code
- [ ] Supporting stepping over, etc.
- [ ] Providing exact source location information for each frame+pc
- [ ] Providing information about variable bindings in each frame
- [ ] ...

## Demo
![stepIn Demo](stepIn.gif)
