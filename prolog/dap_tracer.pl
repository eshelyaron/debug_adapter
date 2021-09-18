:- module(
       dap_tracer,
       [
           dap_trace_interception/4
       ]
   ).

dap_trace_interception(Port, Frame, Choice, Action) :-
    thread_self(DebugeeThreadId),
    thread_send_message(dap_server_debugee_queue, intercepting(DebugeeThreadId, Port, Frame, Choice)),
    dap_tracer_interrupt_server,
    thread_get_message(Action).

dap_tracer_interrupt_server :-
    put_code(dap_server_debugee_out, 3).
