:- initialization(main, main).
:- debug(dap(_)).

main :-
    run_tests.

user:da_server_test_marker_predicate.

cwd(CWD) :-
    source_file(user:da_server_test_marker_predicate, ThisFile),
    file_directory_name(ThisFile, CWD).

:- use_module("../prolog/debug_adapter/client.pl").


user:term_expansion((script(Name) :- Body),
                    (test(Name)   :- da_client:dap_script(Script))) :- comma_list(Body, Script).


:- begin_tests(server).


script(disconnect) :-
    reqres("disconnect"),
    event("exited").

script(initialize) :-
    reqres("initialize"),
    reqres("disconnect"),
    event("exited").

script(launch) :-
    reqres("initialize"),
    call(cwd(CWD)),
    reqres("launch", _{ cwd    : CWD,
                        module : "./target/foo.pl",
                        goal   : "foo"
                      }),
    event("initialized"),
    reqres("disconnect"),
    event("exited").

script(configurationDone) :-
    reqres("initialize"),
    call(cwd(CWD)),
    reqres("launch", _{ cwd    : CWD,
                        module : "./target/foo.pl",
                        goal   : "foo"
                      }),
    event("initialized"),
    reqres("configurationDone"),
    reqres("disconnect"),
    event("exited").

script(sourceBreakpoint) :-
    reqres("initialize"),
    call(cwd(CWD)),
    reqres("launch", _{ cwd    : CWD,
                        module : "./target/breakpoint.pl",
                        goal   : "bp"
                      }),
    event("initialized"),
    reqres("setBreakpoints",
           _{ breakpoints    : [_{line : 8}],
              lines          : [8],
              source         : _{ name : "breakpoint.pl",
                                  path : "./target/breakpoint.pl"
                                },
              sourceModified : false
            },
           _{ breakpoints : [ _{ column    : 4,
                                 endColumn : 17,
                                 endLine   : 8,
                                 id        : 1,
                                 line      : 8,
                                 message   : null,
                                 source    : _{ name   : "breakpoint.pl",
                                                origin : "Static",
                                                path   : _Path
                                              },
                                 verified  : true
                               }
                            ]
            }),
    reqres("configurationDone"),
    event("stopped", _{description:_, hitBreakpointIds:_, reason:"entry", text:_, threadId:Id}),
    reqres("continue", _{threadId: Id}),
    event("stopped", _{description:_, hitBreakpointIds:[1], reason:"breakpoint", text:_, threadId:Id}),
    reqres("disconnect"),
    event("exited").


script(functionBreakpoint) :-
    reqres("initialize"),
    call(cwd(CWD)),
    reqres("launch", _{ cwd    : CWD,
                        module : "./target/breakpoint.pl",
                        goal   : "bp"
                      }),
    event("initialized"),
    reqres("setFunctionBreakpoints",
           _{ breakpoints : [_{name : "is_a/2"}] },
           _{ breakpoints : [_{verified : true}] }),
    reqres("configurationDone"),
    event("stopped", _{description:_, hitBreakpointIds:_, reason:"entry", text:_, threadId:Id}),
    reqres("continue", _{threadId: Id}),
    event("continued"),
    event("stopped", _{description:_, hitBreakpointIds:_, reason:"function breakpoint", text:_, threadId:Id}),
    reqres("disconnect"),
    event("exited").


script(exceptionBreakpoints) :-
    reqres("initialize"),
    call(cwd(CWD)),
    reqres("launch", _{ cwd    : CWD,
                        module : "./target/exception.pl",
                        goal   : "spam"
                      }),
    event("initialized"),
    reqres("setExceptionBreakpoints", _{ filters : ["true"] }),
    reqres("configurationDone"),
    event("stopped", _{description:_, hitBreakpointIds:_, reason:"entry", text:_, threadId:Id}),
    reqres("continue", _{threadId: Id}),
    event("continued"),
    event("stopped", _{description:_, hitBreakpointIds:_, reason:"exception", text:_, threadId:Id}),
    reqres("disconnect"),
    event("exited").


script(voluntaryTrace) :-
    reqres("initialize"),
    call(cwd(CWD)),
    reqres("launch", _{ cwd    : CWD,
                        module : "./target/trace.pl",
                        goal   : "doit"
                      }),
    event("initialized"),
    reqres("configurationDone"),
    event("stopped", _{description:_, hitBreakpointIds:_, reason:"entry", text:_, threadId:Id}),
    reqres("continue", _{threadId: Id}),
    event("continued"),
    event("stopped", _{description:_, hitBreakpointIds:_, reason:"trace", text:_, threadId:Id}),
    reqres("disconnect"),
    event("exited").


script(stepInTargets) :-
    reqres("initialize"),
    call(cwd(CWD)),
    reqres("launch", _{ cwd    : CWD,
                        module : "./target/choice.pl",
                        goal   : "cut_choice_point"
                      }),
    event("initialized"),
    reqres("configurationDone"),
    event("stopped", _{description:_, hitBreakpointIds:_, reason:"entry", text:_, threadId:Id}),
    reqres("stackTrace",
           _{threadId: Id},
           _{stackFrames:[ActiveFrame0|_]}),
    call(get_dict(id, ActiveFrame0, FrameId0)),
    reqres("stepInTargets",
           _{frameId: FrameId0},
           _{targets: [_{id: 0, label: "step"}, _{id: 1, label: "fail"}]}),
    reqres("stepIn", _{threadId: Id}),
    reqres("stepIn", _{threadId: Id}),
    reqres("stepIn", _{threadId: Id}),
    reqres("stepIn", _{threadId: Id}),
    reqres("stepIn", _{threadId: Id}),
    reqres("stepIn", _{threadId: Id}),
    reqres("stackTrace",
           _{threadId: Id},
           _{stackFrames:[ActiveFrame|_]}),
    call(get_dict(id, ActiveFrame, FrameId)),
    reqres("stepInTargets",
           _{frameId: FrameId},
           _{targets: [_{id: 0, label: "step"}]}),
    reqres("disconnect"),
    event("exited").


:- end_tests(server).
