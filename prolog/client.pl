:- module(
       da_client,
       [
           dap_request_response/3,
           dap_request_response/11
       ]
   ).

:- use_module(protocol).

dap_request_response(In, Out, Command) :-
    setting(da_protocol:initial_request_seq, Seq),
    dap_request_response(In, Out, Seq, Command).

dap_request_response(In, Out, Seq, Command) :-
    dap_request_response(In, Out, Seq, Command, null).

dap_request_response(In, Out, Seq, Command, Arguments) :-
    dap_request_response(In, Out, Seq, Command, Arguments, _).

dap_request_response(In, Out, Seq, Command, Arguments, Body) :-
    dap_request_response(In, Out, Seq, Command, Arguments, _Events, Body).

dap_request_response(In, Out, Seq, Command, Arguments, Events, Body) :-
    setting(da_protocol:default_request_timeout, Timeout),
    dap_request_response(In, Out, Seq, Command, Arguments, Events, Body, Timeout).

dap_request_response(In, Out, Seq, Command, Arguments, Events, Body, Timeout) :-
    dap_request_response(In, Out, Seq, Command, Arguments, identity, Events, Success, _Message, Body, Timeout),
    Success = true.

dap_request_response(In, Out, Seq, Command, Arguments, Events, Success, Message, Body, Timeout) :-
    dap_request_response(In, Out, Seq, Command, Arguments, identity, Events, Success, Message, Body, Timeout).

:- meta_predicate dap_request_response(?, ?, ?, ?, ?, 2, ?, ?, ?, ?, ?).

dap_request_response(In, Out, Seq, Command, Arguments, OnEventGoal, Events, Success, Message, Body, Timeout) :-
    debug(dap(client), "Sending request ~w ~w ~w", [Seq, Command, Arguments]),
    dap_request(Out, Seq, Command, Arguments),
    dap_await_response(In, Seq, Command, OnEventGoal, Events, Success, Message, Body, Timeout).

dap_await_response(In, Seq, Command, OnEventGoal, Events, Success, Message, Body, Timeout0) :-
    debug(dap(client), "Awaiting response for another ~w seconds", [Timeout0]),
    get_time(Time0),
    wait_for_input([In], ReadyList, Timeout0),
    get_time(Time),
    (   ReadyList = []  % timeout occured
    ->  Success = timeout
    ;   Timeout is Timeout0 - (Time - Time0),
        dap_read(In, R),
        _{ type : Type} :< R,
        (   Type = "response"
        ->  (   _{ request_seq : Seq, command : Command, success : Success } :< R
            ->  Events = [], Message = R.get(message, null), Body = R.get(body, null)
            ;   dap_await_response(In, Seq, Command, OnEventGoal, Events, Success, Message, Body, Timeout)
            )
        ;   Type = "event"
        ->  _{ seq : EventSeq0, event : EventType0 } :< R,
            EventBody0 = R.get(body, null),
            call(OnEventGoal, event(EventSeq0, EventType0, EventBody0), Events0),
            append(Events0, EventsT, Events),
            dap_await_response(In, Seq, Command, OnEventGoal, EventsT, Success, Message, Body, Timeout)
        )
    ).

identity(E, [E]).
