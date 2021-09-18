:- module(
       dap_debugee,
       [
           dap_debugee/1
       ]
   ).

:- meta_predicate dap_debugee(0).

dap_debugee(Goal) :-
    thread_get_message(_),
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
