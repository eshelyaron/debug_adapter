:- module(
       da_session,
       [
           session_request/5,            % session_request(+Type, +Request, -RequestSeq, +Session0, -Session)
           session_request_response/5,   % session_request_response(+Type, +Request, -Response, +Session0, -Session)
           session_response/5,           % session_response(+Type, +RequestSeq, -Response, +Session0, -Session)
           session_event/4,              % session_event(+Type, -Event, +Session0, -Session)
           session_reverse_request/4,    % session_reverse_request(+Type, -Request, +Session0, -Session)
           session_start/2,              % session_start(+ServerSpec, -Session)
           session_stop/1                % session_stop(+Session)
       ]
   ).

:- use_module(protocol).

:- record(session(in, out, pid, seq=1, seen=[])).

%! session_start(+ServerSpec, -Session) is det.
%
%  Start a new DAP session with server ServerSpec and unify Session with
%  the newly created session's state.
%
%  ServerSpec is a non empty list with head _Exec_ and tail _Args_, where _Exec_
%  specifies the DAP server executable and _Args_ is a list of command line
%  arguments passed to the server's invocation.
session_start([Exec|Args], Session) :-
    process_create(Exec, Args,
                   [ stdin(pipe(O)),
                     stdout(pipe(I)),
                     process(P)
                   ]),
    set_stream(I, buffer(full)),
    set_stream(I, newline(dos)),
    set_stream(O, buffer(false)),
    make_session([in(I), out(O), pid(P)], Session).


%! session_stop(+Session) is det.
%
%  Stop the DAP session Session and the associated DAP server process.
session_stop(Session) :-
    session_in(Session, I),
    session_out(Session, O),
    session_pid(Session, P),
    close(O),
    close(I),
    process_wait(P, Status, [timeout(0)]),
    (   Status == timeout
    ->  process_kill(P)
    ;   true
    ).

session_request(Type, Request, Seq0, Session0, Session) :-
    session_out(Session0, Out),
    session_seq(Session0, Seq0),
    dap_request(Out, Seq0, Type, Request),
    Seq is Seq0 + 1,
    set_seq_of_session(Seq, Session0, Session).

session_request_response(Type, Request, Response, Session0, Session) :-
    session_request(Type, Request, ReqSeq, Session0, Session1),
    session_response(Type, ReqSeq, Response, Session1, Session).

session_response(Type0, ReqSeq, Result, Session, Session) :-
    atom_string(Type0, Type),
    session_already(Session, response(_ReqSeq, ReqSeq, Type, Success, Message, Response)),
    !,
    session_response_result(Success, Message, Response, Result).
session_response(Type, ReqSeq, Result, Session0, Session) :-
    session_next(Session0, Session1),
    session_response(Type, ReqSeq, Result, Session1, Session).

session_event(Type0, Result, Session, Session) :-
    atom_string(Type0, Type),
    session_already(Session, event(_Seq, Type, Result)).
session_event(Type, Result, Session0, Session) :-
    session_next(Session0, Session1),
    session_event(Type, Result, Session1, Session).

session_reverse_request(Type0, Result, Session, Session) :-
    atom_string(Type0, Type),
    session_already(Session, request(_Seq, Type, Result)).
session_reverse_request(Type, Result, Session0, Session) :-
    session_next(Session0, Session1),
    session_reverse_request(Type, Result, Session1, Session).

session_next(Session0, Session) :-
    session_in(Session0, I),
    dap_read(I, R),
    _{ type : Type0 } :< R,
    atom_string(Type, Type0),
    session_see(Type, R, Session0, Session).

session_see(response, R, Session0, Session) :-
    _{ seq: Seq, request_seq : ReqSeq, command : Command, success : Success } :< R,
    (   get_dict(message, R, Message)
    ->  true
    ;   Message = null
    ),
    (   get_dict(body, R, Body)
    ->  true
    ;   Body = null
    ),
    session_seen(Session0, Seen0),
    set_seen_of_session([response(Seq, ReqSeq, Command, Success, Message, Body)|Seen0], Session0, Session).
session_see(event, E, Session0, Session) :-
    _{ seq : Seq, event : Event } :< E,
    (   get_dict(body, E, Body)
    ->  true
    ;   Body = null
    ),
    session_seen(Session0, Seen0),
    set_seen_of_session([event(Seq, Event, Body)|Seen0], Session0, Session).
session_see(request, R, Session0, Session) :-
    _{ seq : Seq, command : Command } :< R,
    (   get_dict(arguments, R, Args)
    ->  true
    ;   Args = null
    ),
    session_seen(Session0, Seen0),
    set_seen_of_session([request(Seq, Command, Args)|Seen0], Session0, Session).

session_response_result(true , _, Result, true:Result).
session_response_result(false, Result, _, false:Result).
session_response_result(null, Result, _, false:Result).

session_already(Session, Eventuality) :-
    session_seen(Session, Seen),
    memberchk(Eventuality, Seen).
