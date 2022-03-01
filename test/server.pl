:- initialization(main, main).

main :-
    debug(dap(_)),
    run_tests.

user:da_server_test_marker_predicate.

dapipe(ServerIn, ServerOut, ClientIn, ClientOut) :-
    pipe(ServerIn, ClientOut),
    pipe(ClientIn, ServerOut),
    set_stream(ClientIn,  buffer(full)),
    set_stream(ClientIn,  encoding(octet)),
    set_stream(ClientIn,  newline(dos)),
    set_stream(ClientOut, buffer(false)),
    set_stream(ClientOut, encoding(octet)).

:- begin_tests(server).

:- use_module("../prolog/debug_adapter/client.pl").
:- use_module("../prolog/debug_adapter/server.pl").

test(disconnect, [ setup(dapipe(ServerIn, ServerOut, ClientIn, ClientOut)),
                   cleanup(( close(ServerIn),
                             close(ClientOut),
                             close(ClientIn),
                             close(ServerOut),
                             (   is_thread(ServerThreadId)
                             ->  thread_signal(ServerThreadId, thread_exit(1))
                             ;   true
                             )
                           )
                          )
                 ]
    ) :-
    thread_create(da_server([in(ServerIn), out(ServerOut)]), ServerThreadId, []),
    dap_request_response(ClientIn, ClientOut, "disconnect"),
    thread_join(ServerThreadId, exited(0)).

test(initialize, [ setup(dapipe(SIn, SOut, CIn, COut)),
                   cleanup(( close(SIn),
                             close(COut),
                             close(CIn),
                             close(SOut),
                             (   is_thread(ServerThreadId)
                             ->  thread_signal(ServerThreadId, thread_exit(1))
                             ;   true
                             )
                           )
                          )
                 ]
    ) :-
    thread_create(da_server([in(SIn), out(SOut)]), ServerThreadId, []),
    dap_request_response(CIn, COut, 1, "initialize", null, Body),
    _{ supportsConfigurationDoneRequest : true } :< Body,
    dap_request_response(CIn, COut, 2, "disconnect"),
    thread_join(ServerThreadId, exited(0)).

test(launch, [ setup(dapipe(SIn, SOut, CIn, COut)),
                   cleanup(( close(SIn),
                             close(COut),
                             close(CIn),
                             close(SOut),
                             (   is_thread(ServerThreadId)
                             ->  thread_signal(ServerThreadId, thread_exit(1))
                             ;   true
                             )
                           )
                          )
                 ]
    ) :-
    thread_create(da_server([in(SIn), out(SOut)]), ServerThreadId, []),
    dap_request_response(CIn, COut, 1, "initialize", null, Body),
    _{ supportsConfigurationDoneRequest : true } :< Body,
    source_file(user:da_server_test_marker_predicate, ThisFile),
    file_directory_name(ThisFile, CWD),
    dap_request_response(CIn, COut, 2, "launch", _{ cwd    : CWD,
                                                    module : "./target/foo.pl",
                                                    goal   : foo
                                                  }, [event(_, "initialized", _)|_],
                         _Body),
    dap_request_response(CIn, COut, 3, "disconnect"),
    thread_join(ServerThreadId, exited(0)).

test(configurationDone, [ setup(dapipe(SIn, SOut, CIn, COut)),
                          cleanup(( close(SIn),
                                    close(COut),
                                    close(CIn),
                                    close(SOut),
                                    (   is_thread(ServerThreadId)
                                    ->  thread_signal(ServerThreadId, thread_exit(1))
                                    ;   true
                                    )
                                  )
                                 )
                        ]
    ) :-
    thread_create(da_server([in(SIn), out(SOut)]), ServerThreadId, []),
    dap_request_response(CIn, COut, 1, "initialize", null, Body),
    _{ supportsConfigurationDoneRequest : true } :< Body,
    source_file(user:da_server_test_marker_predicate, ThisFile),
    file_directory_name(ThisFile, CWD),
    dap_request_response(CIn, COut, 2, "launch", _{ cwd    : CWD,
                                                    module : "./target/foo.pl",
                                                    goal   : foo
                                                  }, [event(_, "initialized", _)|_],
                         _Body),
    dap_request_response(CIn, COut, 3, "configurationDone"),
    dap_request_response(CIn, COut, 4, "disconnect"),
    thread_join(ServerThreadId, E),
    assertion(E == exited(0)).

test(breakpoint, [ setup(dapipe(SIn, SOut, CIn, COut)),
                   cleanup(( close(SIn),
                             close(COut),
                             close(CIn),
                             close(SOut),
                             (   is_thread(ServerThreadId)
                             ->  thread_signal(ServerThreadId, thread_exit(1))
                             ;   true
                             )
                           )
                          )
                 ]
    ) :-
    thread_create(da_server([in(SIn), out(SOut)]), ServerThreadId, []),
    dap_request_response(CIn, COut, 1, "initialize", null, Body),
    _{ supportsConfigurationDoneRequest : true } :< Body,
    source_file(user:da_server_test_marker_predicate, ThisFile),
    file_directory_name(ThisFile, CWD),
    dap_request_response(CIn, COut, 2, "launch", _{ cwd    : CWD,
                                                    module : "./target/breakpoint.pl",
                                                    goal   : bp
                                                  }, [event(_, "initialized", _)|_],
                         _Body),
    dap_request_response(CIn, COut, 3, "setBreakpoints",
                         _{ breakpoints    : [_{line : 8}],
                            lines          : [8],
                            source         : _{ name : "breakpoint.pl",
                                                path : "./target/breakpoint.pl"
                                              },
                            sourceModified : false
                          },
                         _Events,
                         BPBody),
    assertion(BPBody = _{ breakpoints : [ _{ column    : 4,
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
    dap_request_response(CIn, COut, 4, "configurationDone"),
    dap_request_response(CIn, COut, 5, "disconnect"),
    thread_join(ServerThreadId, E),
    assertion(E == exited(0)).

test(functionBreakpoint, [ setup(dapipe(SIn, SOut, CIn, COut)),
                           cleanup(( close(SIn),
                                     close(COut),
                                     close(CIn),
                                     close(SOut),
                                     (   is_thread(ServerThreadId)
                                     ->  thread_signal(ServerThreadId, thread_exit(1))
                                     ;   true
                                     )
                                   )
                                  )
                         ]
    ) :-
    thread_create(da_server([in(SIn), out(SOut)]), ServerThreadId, []),
    dap_request_response(CIn, COut, 1, "initialize", null, Body),
    _{ supportsConfigurationDoneRequest : true } :< Body,
    source_file(user:da_server_test_marker_predicate, ThisFile),
    file_directory_name(ThisFile, CWD),
    dap_request_response(CIn, COut, 2, "launch", _{ cwd    : CWD,
                                                    module : "./target/breakpoint.pl",
                                                    goal   : bp
                                                  }, [event(_, "initialized", _)|_],
                         _Body),
    dap_request_response(CIn, COut, 3, "setFunctionBreakpoints",
                         _{ breakpoints    : [_{name : "is_a/2"}] },
                         _Events,
                         BPBody),
    assertion(BPBody = _{ breakpoints : [ _{ verified  : true } ] }),
    dap_request_response(CIn, COut, 4, "configurationDone", null, ConfigDoneEvents, _),
    (   memberchk(event(_, "stopped", StoppedBody0), ConfigDoneEvents)
    ->  true
    ;   dap_await_event(CIn, "stopped", StoppedBody0)
    ),
    StoppedBody0 = _{description:_, hitBreakpointIds:_, reason:"entry", text:_, threadId:Id},
    dap_request_response(CIn, COut, 5, "continue", _{threadId: Id}, Events0, _Body0),
    (   memberchk(event(_, "continued", _), Events0)
    ->  (   memberchk(event(_, "stopped", StoppedBody), Events0)
        ->  true
        ;   dap_await_event(CIn, "stopped", StoppedBody)
        )
    ;   dap_await_event(CIn, "continued"),
        dap_await_event(CIn, "stopped", StoppedBody)
    ),
    assertion(StoppedBody = _{description:_, hitBreakpointIds:_, reason:"function breakpoint", text:_, threadId:Id}),
    dap_request_response(CIn, COut, 6, "disconnect"),
    thread_join(ServerThreadId, E),
    assertion(E == exited(0)).

test(voluntarilyTrace, [ setup(dapipe(SIn, SOut, CIn, COut)),
                         cleanup(( close(SIn),
                                   close(COut),
                                   close(CIn),
                                   close(SOut),
                                   (   is_thread(ServerThreadId)
                                   ->  thread_signal(ServerThreadId, thread_exit(1))
                                   ;   true
                                   )
                                 )
                                )
                       ]
    ) :-
    thread_create(da_server([in(SIn), out(SOut)]), ServerThreadId, []),
    dap_request_response(CIn, COut, 1, "initialize", null, Body),
    _{ supportsConfigurationDoneRequest : true } :< Body,
    source_file(user:da_server_test_marker_predicate, ThisFile),
    file_directory_name(ThisFile, CWD),
    dap_request_response(CIn, COut, 2, "launch", _{ cwd    : CWD,
                                                    module : "./target/trace.pl",
                                                    goal   : doit
                                                  }, [event(_, "initialized", _)|_],
                         _Body),
        dap_request_response(CIn, COut, 4, "configurationDone", null, ConfigDoneEvents, _),
    (   memberchk(event(_, "stopped", StoppedBody0), ConfigDoneEvents)
    ->  true
    ;   dap_await_event(CIn, "stopped", StoppedBody0)
    ),
    StoppedBody0 = _{description:_, hitBreakpointIds:_, reason:"entry", text:_, threadId:Id},
    dap_request_response(CIn, COut, 5, "continue", _{threadId: Id}, Events0, _Body0),
    (   memberchk(event(_, "continued", _), Events0)
    ->  (   memberchk(event(_, "stopped", StoppedBody), Events0)
        ->  true
        ;   dap_await_event(CIn, "stopped", StoppedBody)
        )
    ;   dap_await_event(CIn, "continued"),
        dap_await_event(CIn, "stopped", StoppedBody)
    ),
    assertion(StoppedBody = _{description:_, hitBreakpointIds:_, reason:"trace", text:_, threadId:Id}),
    dap_request_response(CIn, COut, 6, "disconnect"),
    thread_join(ServerThreadId, E),
    assertion(E == exited(0)).

test(stepInTargets, [ setup(dapipe(SIn, SOut, CIn, COut)),
                      cleanup(( close(SIn),
                                close(COut),
                                close(CIn),
                                close(SOut),
                                (   is_thread(ServerThreadId)
                                ->  thread_signal(ServerThreadId, thread_exit(1))
                                ;   true
                                )
                              )
                             )
                    ]
    ) :-
    thread_create(da_server([in(SIn), out(SOut)]), ServerThreadId, []),
    dap_request_response(CIn, COut, 1, "initialize", null, Body),
    _{ supportsConfigurationDoneRequest : true } :< Body,
    source_file(user:da_server_test_marker_predicate, ThisFile),
    file_directory_name(ThisFile, CWD),
    dap_request_response(CIn, COut, 2, "launch", _{ cwd    : CWD,
                                                    module : "./target/choice.pl",
                                                    goal   : cut_choice_point
                                                  }, [event(_, "initialized", _)|_],
                         _Body),
        dap_request_response(CIn, COut, 4, "configurationDone", null, ConfigDoneEvents, _),
    (   memberchk(event(_, "stopped", StoppedBody0), ConfigDoneEvents)
    ->  true
    ;   dap_await_event(CIn, "stopped", StoppedBody0)
    ),
    StoppedBody0 = _{description:_, hitBreakpointIds:_, reason:"entry", text:_, threadId:Id},
    dap_request_response(CIn, COut, 5, "stackTrace", _{threadId: Id}, Events0, Body0),
    get_dict(stackFrames, Body0, [ActiveFrame|_]),
    get_dict(id, ActiveFrame, FrameId),
    dap_request_response(CIn, COut, 6, "stepInTargets", _{frameId: FrameId}, Events1, Body1),
    assertion(Body1 = _{targets:[_{id:0, label:"step"}, _{id:1, label:"fail"}]}),
    dap_request_response(CIn, COut, 7, "stepIn", _{threadId: Id}),
    dap_request_response(CIn, COut, 8, "stepIn", _{threadId: Id}),
    dap_request_response(CIn, COut, 9, "stepIn", _{threadId: Id}),
    dap_request_response(CIn, COut, 10, "stepIn", _{threadId: Id}),
    dap_request_response(CIn, COut, 11, "stepIn", _{threadId: Id}),
    dap_request_response(CIn, COut, 12, "stackTrace", _{threadId: Id}, Events2, Body2),
    get_dict(stackFrames, Body2, [ActiveFrame2|_]),
    get_dict(id, ActiveFrame2, FrameId2),
    dap_request_response(CIn, COut, 13, "stepInTargets", _{frameId: FrameId2}, Events3, Body3),
    assertion(Body3 = _{targets:[_{id:0, label:"step"}]}),
    dap_request_response(CIn, COut, 14, "disconnect"),
    thread_join(ServerThreadId, E),
    assertion(E == exited(0)).

:- end_tests(server).
