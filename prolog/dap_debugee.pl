:- module(
       dap_debugee,
       [
           dap_debugee/1
       ]
   ).

:- use_module(dap_tracer).
:- meta_predicate dap_debugee(0).

dap_debugee(ModulePath, Goal) :-
    thread_get_message(_), % wait for a trigger from the server
    dap_tracer_start,
    catch( ( trace, (  Goal
                    -> notrace, put_code(dap_server_debugee_out, 0)
                    ;  notrace, put_code(dap_server_debugee_out, 1)
                    )
           ),
           _Catcher,
           ( notrace,
             put_code(dap_server_debugee_out, 2)
           )
         ).
