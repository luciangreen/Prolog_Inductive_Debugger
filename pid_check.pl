% Simple PID wrapper for shell script
% Author: luciangreen
% Date: 2025-06-21 21:31:10

% Load all modules first
:- use_module(library(lists)).
:- consult(pid).
:- use_module(proof_trace).
:- use_module(bug_detector).

% Main analysis predicate - called from command line
analyze(File) :-
    analyze(File, []).

% With options
analyze(File, Options) :-
    format('~nPROLOG INDUCTIVE DEBUGGER~n'),
    format('---------------------~n'),
    format('Analyzing file: ~w~n', [File]),
    format('Options: ~w~n~n', [Options]),
    
    % Convert string options to PID options
    process_options(Options, PidOptions),
    
    % Run the analysis
    pid:pid_main(File, PidOptions).

% Process options
process_options([], []).
process_options(['--tail'|Rest], [check_tail_recursion|Options]) :- !,
    process_options(Rest, Options).
process_options(['--proof'|Rest], [generate_proof|Options]) :- !,
    process_options(Rest, Options).
process_options(['--verbose'|Rest], [verbose|Options]) :- !,
    process_options(Rest, Options).
process_options([Unknown|Rest], Options) :-
    format('Warning: Ignoring unknown option: ~w~n', [Unknown]),
    process_options(Rest, Options).