% Main CLI wrapper for Prolog Inductive Debugger
:- initialization(pid_cli_main, main).

% Make sure all required modules are loaded
:- use_module(library(lists)).
:- consult(pid).
:- use_module(proof_trace).
:- use_module(bug_detector).

% Main entry point - renamed to avoid conflict
pid_cli_main(Args) :-
    % Parse command-line arguments
    (Args = [] -> 
        print_usage,
        halt(1)
    ;
        parse_args(Args, File, Options),
        run_pid(File, Options)
    ).

% Parse arguments
parse_args([File|Rest], File, Options) :-
    parse_options(Rest, Options).

parse_options([], []).
parse_options(['--tail'|Rest], [check_tail_recursion|Options]) :- !,
    parse_options(Rest, Options).
parse_options(['--proof'|Rest], [generate_proof|Options]) :- !,
    parse_options(Rest, Options).
parse_options(['--verbose'|Rest], [verbose|Options]) :- !,
    parse_options(Rest, Options).
parse_options([_|Rest], Options) :-
    parse_options(Rest, Options).

% Run PID
run_pid(File, Options) :-
    format('Running Prolog Inductive Debugger on: ~w~n', [File]),
    % Make sure the file exists
    (exists_file(File) -> 
        % Run the actual analysis
        pid:pid_main(File, Options),
        format('Analysis complete.~n')
    ;
        format('Error: File not found: ~w~n', [File]),
        halt(1)
    ).

% Print usage
print_usage :-
    format('Usage: swipl -s pid_main.pl -- <file.pl> [options]~n', []),
    format('Options:~n', []),
    format('  --tail     Check for tail recursion~n', []),
    format('  --proof    Generate proof traces~n', []),
    format('  --verbose  Verbose output~n', []).