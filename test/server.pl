:- initialization(main, main).
:- debug(dap(_)).

main :-
    run_tests.

user:da_server_test_marker_predicate.

cwd(CWD) :-
    source_file(user:da_server_test_marker_predicate, ThisFile),
    file_directory_name(ThisFile, CWD).

:- use_module(library(debug_adapter/script)).

:- begin_tests(server).

test(scripts, [forall((cwd(CWD), directory_member(CWD, Path, [recursive(true), extensions([dapscript])])))]) :-
    debug(dap(test), "Running dapscript ~w", [Path]),
    run_script(Path, [bindings(['WD'=CWD])]).

:- end_tests(server).
