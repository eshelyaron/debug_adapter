:- begin_tests(server).

:- use_module("../prolog/client.pl").
:- use_module("../prolog/server.pl").

test(disconnect, [ setup(( pipe(ServerIn, ClientOut),
                           pipe(ClientIn, ServerOut),
                           set_stream(ClientIn,  buffer(full)),
                           set_stream(ClientIn,  encoding(octet)),
                           set_stream(ClientIn,  newline(dos)),
                           set_stream(ClientOut, buffer(false)),
                           set_stream(ClientOut, encoding(octet))
                         )
                        ),
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

:- end_tests(server).
