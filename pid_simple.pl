% Simple entry point for Prolog Inductive Debugger
% Author: luciangreen
% Date: 2025-06-21 21:28:11

% Main entry point - no argument parsing needed
:- initialization(main, main).

main :-
    % Load required modules
    ensure_loaded(pid),
    ensure_loaded(proof_trace),
    ensure_loaded(bug_detector),
    use_module(library(lists)),
    
    % Get command line arguments
    current_prolog_flag(argv, AllArgs),
    % Skip script name
    (append(_, ['pid_simple.pl', FileName | Options], AllArgs) ->
        % Good, we have the expected args
        true
    ;
        % Just take the first args we find
        AllArgs = [FileName | Options]
    ),
    
    % Convert options to pid options
    convert_options(Options, PidOptions),
    
    % Run analysis
    format('Running analysis on ~w with options ~w~n', [FileName, PidOptions]),
    pid:pid_main(FileName, PidOptions),
    
    % Exit cleanly
    halt(0).

% Convert command line options to PID options
convert_options([], []).
convert_options(['--tail'|Rest], [check_tail_recursion|Options]) :- !,
    convert_options(Rest, Options).
convert_options(['--proof'|Rest], [generate_proof|Options]) :- !,
    convert_options(Rest, Options).
convert_options(['--verbose'|Rest], [verbose|Options]) :- !,
    convert_options(Rest, Options).
convert_options([Unknown|Rest], Options) :-
    format('Warning: Ignoring unknown option: ~w~n', [Unknown]),
    convert_options(Rest, Options).