% PID Integration Module
:- module(pid_integration, [run_pid/2]).
:- use_module(proof_trace).

% Run PID on a file with options
run_pid(File, Options) :-
    format('Running Prolog Inductive Debugger on ~w~n', [File]),
    
    % Parse the file
    parse_file(File, Predicates),
    
    % Analyze predicates
    analyze_predicates(Predicates, Options, Results),
    
    % Generate and print report
    generate_report(Results, Options),
    
    % If requested, generate proof traces
    (member(generate_proof, Options) ->
        generate_proof_traces(Results, ProofTraces),
        print_proof_traces(ProofTraces)
    ;   true),
    
    % Return results
    true.

% Generate proof traces for all results
generate_proof_traces([], []).
generate_proof_traces([result(Name/Arity, Checks)|Results], [Trace|Traces]) :-
    % Extract structure from checks (we'd need to modify analyze_predicates to store this)
    member(structure(Structure), Checks),
    generate_proof_trace(Structure, Trace),
    generate_proof_traces(Results, Traces).

% Print all proof traces
print_proof_traces([]).
print_proof_traces([Trace|Traces]) :-
    print_proof_trace(Trace),
    print_proof_traces(Traces).