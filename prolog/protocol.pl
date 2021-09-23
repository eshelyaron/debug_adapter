:- module(
       da_protocol,
       [
           dap_read/2,
           dap_error/5,
           dap_response/4,
           dap_response/5,
           dap_response/7,
           dap_request/3,
           dap_request/4,
           dap_event/3,
           dap_event/4,
           dap_message/4
       ]
   ).

:- use_module(library(http/json)).
:- use_module(library(settings)).

:- setting(default_request_timeout,
           number,
           5,
           'Default time to wait for DAP server to respond, given in seconds').
:- setting(initial_request_seq,
           integer,
           1,
           'Initial `seq` field value for DAP requests').

dap_read(In, Message) :-
    read_line_to_string(In, Line),
    sub_string(Line, 16, _, 0, ContentLengthString), % string_length("Content-Length: ", 16).
    number_string(ContentLength, ContentLengthString),
    read_line_to_string(In, ""),
    read_string(In, ContentLength, Serialized),
    atom_json_dict(Serialized, Message, []).

dap_error(Out, Seq, RequestSeq, Command, Message) :-
    dap_response(Out, Seq, RequestSeq, Command, false, Message, null).

dap_response(Out, Seq, RequestSeq, Command) :-
    dap_response(Out, Seq, RequestSeq, Command, null).

dap_response(Out, Seq, RequestSeq, Command, Body) :-
    dap_response(Out, Seq, RequestSeq, Command, true, null, Body).

dap_response(Out, Seq, RequestSeq, Command, Success, Message, Body) :-
    dap_message(Out, Seq, "response", _{ request_seq: RequestSeq,
                                                       success    : Success,
                                                       command    : Command,
                                                       message    : Message,
                                                       body       : Body
                                       }
               ).

dap_event(Out, Seq, Event) :-
    dap_event(Out, Seq, Event, null).

dap_event(Out, Seq, Event, Body) :-
    dap_message(Out, Seq, "event", _{ event: Event,
                                      body : Body
                                    }
               ).

dap_request(Out, Seq, Command) :-
    dap_request(Out, Seq, Command, null).

dap_request(Out, Seq, Command, Arguments) :-
    dap_message(Out, Seq, "request", _{ command : Command,
                                        arguments : Arguments
                                      }
               ).

dap_message(Out, Seq, Type, Rest) :-
    put_dict(_{ seq : Seq,
                type: Type
              },
             Rest,
             Message
            ),
    atom_json_dict(Serialized, Message, []),
    dap_serialized_content(Out, Serialized).

dap_serialized_content(Out, Content) :-
    string_length(Content, ContentLength),
    format(Out, "Content-Length: ~w\r\n\r\n", [ContentLength]),
    format(Out, "~w", Content).
