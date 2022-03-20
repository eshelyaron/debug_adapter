#!/usr/bin/env swipl

:- initialization(main, main).

:- nodebug(dap(main)).

:- use_module(server).
:- use_module(library(swipl_debug_adapter)).

main(Argv) :-
    OptsSpec =
    [ [opt(version),
       default(false),
       type(boolean),
       shortflags(['V']),
       longflags([version]),
       help('Print version information')
      ],
      [opt(help),
       default(false),
       type(boolean),
       shortflags([h]),
       longflags([help]),
       help('Print usage information')
      ],
      [opt(debug),
       default(true),
       type(boolean),
       shortflags([d]),
       longflags([debug]),
       help('Print debug messages to stderr')
      ],
      [opt(timeout),
       default(3600),
       type(integer),
       shortflags(['T']),
       longflags([timeout]),
       help('Set a global timeout after which the server aborts. Useful for CI')
      ]
    ],

    opt_parse(OptsSpec, Argv, Opts, _PosArgs),

    ( option(version(true), Opts) -> pack_info(debug_adapter)
    ; option(help(   true), Opts) -> opt_help(OptsSpec, Help), write(Help)
    ; set_prolog_flag(toplevel_prompt, ''),
      (   option(debug(  true), Opts)
      ->  debug(dap(_))
      ;   true
      ),
      current_input(In),
      current_output(Out),
      option(timeout(Timeout), Opts),
      da_server([in(In), out(Out), on_command(swipl_debug_adapter_command_callback), timeout(Timeout)])
    ).
