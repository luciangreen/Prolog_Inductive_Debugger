% Unified driver script for Prolog Inductive Debugger
% Author: luciangreen
% Date: 2025-06-21 21:20:13

% Use proper initialization hook to access command line arguments
:- initialization(run_main, main).

% Main entry point that properly handles command line arguments
run_main :-
    % Load required modules
    consult(pid),
    use_module(proof_trace),
    use_module(bug_detector),
    
    % Get command-line arguments
    current_prolog_flag(argv, Argv),
    
    % Print the raw arguments for debugging
    format('DEBUG - Raw arguments: ~w~n', [Argv]),
    
    % Process arguments - simplest approach
    (Argv = [] ->
        % No arguments provided
        print_usage
    ;
        process_args(Argv)
    ),
    
    % Exit cleanly
    halt(0).

% Process the argument list
process_args(Argv) :-
    % Find the script name and anything after it
    (append(_, ['run_pid.pl', File | Options], Argv) ->
        % Got file and options
        process_file_and_options(File, Options)
    ;
        % Just use first argument as file
        Argv = [File | Options],
        process_file_and_options(File, Options)
    ).

% Process the file and options
process_file_and_options(File, Options) :-
    % Check if file exists
    format('Processing file ~w with options ~w~n', [File, Options]),
    (exists_file(File) ->
        % Process options
        parse_options(Options, PidOptions),
        
        % Run the analysis
        format('Running analysis...~n'),
        pid:pid_main(File, PidOptions)
    ;   
        % File not found
        format('Error: File not found: ~w~n', [File]),
        halt(1)
    ).

% Parse options from command line strings to PID option terms
parse_options([], []).
parse_options(['--tail'|Rest], [check_tail_recursion|Options]) :- !,
    parse_options(Rest, Options).
parse_options(['--proof'|Rest], [generate_proof|Options]) :- !,
    parse_options(Rest, Options).
parse_options(['--verbose'|Rest], [verbose|Options]) :- !,
    parse_options(Rest, Options).
parse_options([Unknown|Rest], Options) :-
    format('Warning: Ignoring unknown option: ~w~n', [Unknown]),
    parse_options(Rest, Options).

% Print usage information
print_usage :-
    format('Prolog Inductive Debugger (PID) - Static Analysis Tool~n'),
    format('Author: luciangreen~n'),
    format('Date: 2025-06-21 21:20:13~n'),
    format('~n'),
    format('Usage: swipl -s run_pid.pl <file.pl> [options]~n'),
    format('   or: swipl -s run_pid.pl -- <file.pl> [options]~n'),
    format('~n'),
    format('Options:~n'),
    format('  --tail     Check for tail recursion~n'),
    format('  --proof    Generate proof traces~n'),
    format('  --verbose  Verbose output~n'),
    format('~n'),
    format('Example: swipl -s run_pid.pl examples/reverse.pl --tail --proof~n'),
    format('~n').