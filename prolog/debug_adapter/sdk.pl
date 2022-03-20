:- module(
       da_sdk,
       [
           da_sdk_response/3,  % da_sdk_response(+Handle, +RequestSeq, +Command)
           da_sdk_response/4,  % da_sdk_response(+Handle, +RequestSeq, +Command, +Body)
           da_sdk_error/4,     % da_sdk_response(+Handle, +RequestSeq, +Command, +Message)
           da_sdk_event/2,     % da_sdk_response(+Handle, +Type)
           da_sdk_event/3,     % da_sdk_response(+Handle, +Type, +Body)
           da_sdk_stop/1       % da_sdk_response(+Handle)
       ]
   ).


/** <module> SWI-Prolog Debug Adapter Protocol SDK

Facilities for implementing DAP servers. The predicates in this module
are designed to be called during a DAP session to modify the state of
the session or to communicate with the DAP client.

@see Option =on_command= of =da_server/1=.
*/


da_sdk_action(Handle, Action) :-
    thread_send_message(Handle, action(Action)).


da_sdk_response(Handle, ReqSeq, Command) :-
    da_sdk_response(Handle, ReqSeq, Command, null).

da_sdk_response(Handle, ReqSeq, Command, Body) :-
    da_sdk_action(Handle, response(ReqSeq, Command, Body)).


da_sdk_error(Handle, ReqSeq, Command, Message) :-
    da_sdk_action(Handle, error(ReqSeq, Command, Message)).


da_sdk_event(Handle, Type) :-
    da_sdk_event(Handle, Type, null).

da_sdk_event(Handle, Type, Body) :-
    da_sdk_action(Handle, event(Type, Body)).


da_sdk_request(Handle, Type, Body) :-
    da_sdk_action(Handle, request(Type, Body)).

da_sdk_stop(Handle) :-
    da_sdk_action(Handle, stop).
