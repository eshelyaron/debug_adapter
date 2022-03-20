:- initialization(main, main).
:- debug(dap(_)).

main :-
    run_tests.

user:da_server_test_marker_predicate.

cwd(CWD) :-
    source_file(user:da_server_test_marker_predicate, ThisFile),
    file_directory_name(ThisFile, CWD).

:- use_module(library(debug_adapter/client)).
:- use_module(library(debug_adapter/script)).


user:term_expansion((script(Name) :- Body),
                    (test(Name)   :- da_client:dap_script(Script))) :- comma_list(Body, Script).


:- begin_tests(server).


%% script(stepInTargets) :-
%%     reqres("initialize"),
%%     call(cwd(CWD)),
%%     reqres("launch", _{ cwd    : CWD,
%%                         module : "./target/choice.pl",
%%                         goal   : "cut_choice_point"
%%                       }),
%%     event("initialized"),
%%     reqres("configurationDone"),
%%     event("stopped", _{description:_, hitBreakpointIds:_, reason:"entry", text:_, threadId:Id}),
%%     reqres("stackTrace",
%%            _{threadId: Id},
%%            _{stackFrames:[ActiveFrame0|_]}),
%%     call(get_dict(id, ActiveFrame0, FrameId0)),
%%     reqres("stepInTargets",
%%            _{frameId: FrameId0},
%%            _{targets: [_{id: 0, label: "step"}, _{id: 1, label: "fail"}]}),
%%     reqres("stepIn", _{threadId: Id}),
%%     reqres("stepIn", _{threadId: Id}),
%%     reqres("stepIn", _{threadId: Id}),
%%     reqres("stepIn", _{threadId: Id}),
%%     reqres("stepIn", _{threadId: Id}),
%%     reqres("stepIn", _{threadId: Id}),
%%     reqres("stackTrace",
%%            _{threadId: Id},
%%            _{stackFrames:[ActiveFrame|_]}),
%%     call(get_dict(id, ActiveFrame, FrameId)),
%%     reqres("stepInTargets",
%%            _{frameId: FrameId},
%%            _{targets: [_{id: 0, label: "step"}]}),
%%     reqres("disconnect"),
%%     event("exited").

test(scripts, [forall((cwd(CWD), directory_member(CWD, Path, [recursive(true), extensions([dapscript])])))]) :-
    debug(dap(test), "Running dapscript ~w", [Path]),
    run_script(Path, [bindings(['WD'=CWD])]).

:- end_tests(server).
