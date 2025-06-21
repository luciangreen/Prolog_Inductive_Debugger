% Command-line interface for PID
:- initialization(main, main).

% Main entry point
main(Args) :-
    % Set up dependencies
    ensure_loaded('pid.pl'),
    ensure_loaded('proof_trace.pl'),
    ensure_loaded('bug_detector.pl'),
    
    % Parse command-line arguments
    parse_arguments(Args, File, Options),
    
    % Run PID
    (   File \= ''
    ->  pid_main(File, Options)
    ;   print_usage
    ).

% Parse command-line arguments
parse_arguments([], '', []) :- !.
parse_arguments([File|Args], File, Options) :-
    parse_options(Args, Options).

parse_options([], []).
parse_options(['--tail'|Args], [check_tail_recursion|Options]) :- !,
    parse_options(Args, Options).
parse_options(['--proof'|Args], [generate_proof|Options]) :- !,
    parse_options(Args, Options).
parse_options(['--verbose'|Args], [verbose|Options]) :- !,
    parse_options(Args, Options).
parse_options([_|Args], Options) :-
    parse_options(Args, Options).

% Print usage information
print_usage :-
    format('Usage: pid_cli.pl <file.pl> [options]~n', []),
    format('Options:~n', []),
    format('  --tail    Check for tail recursion~n', []),
    format('  --proof   Generate proof traces~n', []),
    format('  --verbose Verbose output~n', []).