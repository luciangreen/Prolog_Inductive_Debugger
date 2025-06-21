% Command-line interface for PID
:- initialization(pid_main, main).

% Import the required modules
:- ensure_loaded(pid).
:- ensure_loaded(proof_trace).
:- ensure_loaded(bug_detector).

pid_main(Args) :-
    % Parse command-line arguments
    parse_cli_arguments(Args, File, Options),
    
    % Run PID
    (   File \= ''
    ->  pid_main(File, Options)
    ;   print_cli_usage
    ).

% Parse command-line arguments
parse_cli_arguments([], '', []) :- !.
parse_cli_arguments([File|Args], File, Options) :-
    parse_cli_options(Args, Options).

parse_cli_options([], []).
parse_cli_options(['--tail'|Args], [check_tail_recursion|Options]) :- !,
    parse_cli_options(Args, Options).
parse_cli_options(['--proof'|Args], [generate_proof|Options]) :- !,
    parse_cli_options(Args, Options).
parse_cli_options(['--verbose'|Args], [verbose|Options]) :- !,
    parse_cli_options(Args, Options).
parse_cli_options([_|Args], Options) :-
    parse_cli_options(Args, Options).

% Print usage information
print_cli_usage :-
    format('Usage: swipl -s pid_cli.pl -- <file.pl> [options]~n', []),
    format('Options:~n', []),
    format('  --tail    Check for tail recursion~n', []),
    format('  --proof   Generate proof traces~n', []),
    format('  --verbose Verbose output~n', []).