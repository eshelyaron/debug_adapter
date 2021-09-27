#!/usr/bin/env swipl

:- initialization(main, main).

:- use_module(server).

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
      ]
    ],

    opt_parse(OptsSpec, Argv, Opts, _PosArgs),

    ( option(version(true), Opts) -> version(Version), format("SWI-Prolog Debug Adapter Version ~w~n", [Version])
    ; option(help(   true), Opts) -> opt_help(OptsSpec, Help), write(Help)
    ; set_prolog_flag(toplevel_prompt, ''),
      ( option(debug(  true), Opts) -> debug(dap(tracer)); true ),
      current_input(In),
      current_output(Out),
      da_server([in(In), out(Out)])
    ).
