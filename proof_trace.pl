% Prolog Inductive Debugger (PID)
% Author: luciangreen (lg3)
% Date: 2025-06-21 21:52:44

% Declare the module before any other directives
:- module(pid, [pid_main/2]).

% Make sure we use the required libraries
:- use_module(library(lists)).

% Import required modules
:- use_module(proof_trace, [generate_proof_trace/2]).
:- use_module(bug_detector, [detect_bugs/6]).

% Main entry point
pid_main(File, Options) :-
    format('Loading file: ~w~n', [File]),
    % Check if file exists and is readable
    (access_file(File, read) -> 
        read_file(File, Predicates),
        analyze_and_report(Predicates, Options)
    ;
        format('Error: Cannot read file ~w~n', [File])
    ).

% Read and analyze predicates
analyze_and_report(Predicates, Options) :-
    (Predicates = [] -> 
        format('No predicates found in file.~n')
    ;
        length(Predicates, NumPredicates),
        format('Found ~d predicates to analyze~n', [NumPredicates]),
        analyze_predicates(Predicates, Options, Results),
        generate_report(Results, Options)
    ).

% Read file and extract predicates
read_file(File, Predicates) :-
    catch(
        (
            % Read whole file into a string
            read_file_to_string(File, String, []),
            % Parse the string into clauses
            parse_clauses_from_string(String, Clauses),
            % Group clauses by predicate
            group_clauses_by_predicate(Clauses, Predicates)
        ),
        Error,
        (
            format('Error parsing file: ~w~n', [Error]),
            Predicates = []
        )
    ).

% Parse clauses from a string
parse_clauses_from_string(String, Clauses) :-
    setup_call_cleanup(
        open_string(String, Stream),
        read_clauses_from_stream(Stream, Clauses),
        close(Stream)
    ).

% Read all clauses from a stream
read_clauses_from_stream(Stream, Clauses) :-
    read_term(Stream, Term, []),
    (Term == end_of_file ->
        Clauses = []
    ;
        process_term(Term, Clause),
        Clauses = [Clause|Rest],
        read_clauses_from_stream(Stream, Rest)
    ).

% Process a term into a clause
process_term((Head :- Body), clause(Head, Body)) :- !.
process_term(Fact, clause(Fact, true)).

% Group clauses by predicate
group_clauses_by_predicate(Clauses, Predicates) :-
    % Create a dictionary to group clauses
    empty_assoc(Empty),
    foldl(add_clause_to_group, Clauses, Empty, GroupMap),
    extract_predicate_groups(GroupMap, Predicates).

% Extract predicate groups from assoc
extract_predicate_groups(GroupMap, Predicates) :-
    assoc_to_list(GroupMap, GroupPairs),
    maplist(pair_to_predicate, GroupPairs, Predicates).

% Convert a key-value pair to a predicate
pair_to_predicate(Key-Clauses, predicate(Key, Clauses)).

% Add a clause to its predicate group
add_clause_to_group(clause(Head, Body), GroupMapIn, GroupMapOut) :-
    functor(Head, Name, Arity),
    Key = Name/Arity,
    (get_assoc(Key, GroupMapIn, ExistingClauses) ->
        % Add to existing group
        put_assoc(Key, GroupMapIn, [clause(Head, Body)|ExistingClauses], GroupMapOut)
    ;
        % Create new group
        put_assoc(Key, GroupMapIn, [clause(Head, Body)], GroupMapOut)
    ).

% Analyze all predicates
analyze_predicates([], _, []).
analyze_predicates([Predicate|Predicates], Options, [Result|Results]) :-
    analyze_single_predicate(Predicate, Options, Result),
    analyze_predicates(Predicates, Options, Results).

% Analyze a single predicate
analyze_single_predicate(predicate(Name/Arity, Clauses), Options, result(Name/Arity, Analysis)) :-
    % Print what we're analyzing
    length(Clauses, NumClauses),
    format('Analyzing ~w/~d with ~d clauses...~n', [Name, Arity, NumClauses]),
    
    % Build predicate dictionary for recursive call analysis
    collect_all_predicates(Options, PredDict),
    
    % Classify clauses as base cases or recursive cases with predicate info
    extract_structure(Name/Arity, Clauses, PredDict, Structure),
    
    % Run analysis
    Structure = structure(Name/Arity, BaseCases, RecCases),
    
    % Print partition results
    length(BaseCases, NumBaseCases),
    length(RecCases, NumRecCases),
    format('  Found ~d base cases and ~d recursive cases~n', [NumBaseCases, NumRecCases]),
    
    % Validate base cases
    validate_base_cases(BaseCases, BaseCaseValid),
    
    % Validate inductive steps
    validate_inductive_steps(RecCases, Name/Arity, InductiveStepsValid),
    
    % Check termination
    (BaseCaseValid == valid, InductiveStepsValid == valid ->
        TerminationValid = valid
    ;
        TerminationValid = invalid
    ),
    
    % Check for tail recursion if requested
    (member(check_tail_recursion, Options) ->
        check_tail_recursion(RecCases, Name/Arity, TailRecursiveValid)
    ;   
        TailRecursiveValid = not_checked
    ),
    
    % Detect bugs
    bug_detector:detect_bugs(Structure, BaseCaseValid, InductiveStepsValid, TerminationValid, TailRecursiveValid, Bugs),
    
    % Generate proof trace if requested
    (member(generate_proof, Options) ->
        proof_trace:generate_proof_trace(Structure, ProofTrace),
        ProofTraceResult = proof_trace(ProofTrace)
    ;
        ProofTraceResult = none
    ),
    
    % Combine results
    Analysis = [
        base_case(BaseCaseValid),
        inductive_step(InductiveStepsValid),
        termination(TerminationValid),
        tail_recursion(TailRecursiveValid),
        bugs(Bugs),
        proof_trace_result(ProofTraceResult)
    ].

% Collect all predicates into a dictionary for recursive analysis
collect_all_predicates(_, dict{}).  % Simplified version for now

% Extract recursive structure from clauses
extract_structure(Name/Arity, Clauses, PredDict, structure(Name/Arity, BaseCases, RecCases)) :-
    % Better classification that considers indirect recursion
    partition_clauses_better(Clauses, Name/Arity, PredDict, BaseCases, RecCases).

% Improved clause partitioning that checks for indirect recursion
partition_clauses_better([], _, _, [], []).
partition_clauses_better([Clause|Clauses], Name/Arity, PredDict, BaseCases, RecCases) :-
    % Consider both direct recursion and calls to helper predicates
    (is_true_base_case(Clause, Name/Arity, PredDict) ->
        % This is a true base case
        BaseCases = [Clause|RestBaseCases],
        RecCases = RestRecCases
    ;
        % This is a recursive case or a wrapper
        BaseCases = RestBaseCases,
        RecCases = [Clause|RestRecCases]
    ),
    partition_clauses_better(Clauses, Name/Arity, PredDict, RestBaseCases, RestRecCases).

% Check if clause is a true base case (not just a wrapper)
is_true_base_case(clause(_, Body), Name/Arity, _PredDict) :-
    % For now, we'll consider a clause a base case if it doesn't directly
    % contain a call to the same predicate or another predicate that 
    % could lead to recursion
    \+ contains_recursive_call(Body, Name/Arity),
    
    % Additionally, check that the body is "simple enough" to be a base case
    is_simple_body(Body).

% Check if body is simple enough to be considered a base case
is_simple_body(true) :- !.
is_simple_body((A, B)) :- !,
    is_simple_body(A),
    is_simple_body(B).
is_simple_body(Goal) :-
    % A goal is simple if it's a built-in or doesn't call other predicates
    % that might lead to indirect recursion
    functor(Goal, Name, _),
    simple_predicate(Name).

% List of predicates considered "simple" for base cases
simple_predicate('=').
simple_predicate('is').
simple_predicate('==').
simple_predicate('\\==').
simple_predicate('>').
simple_predicate('<').
simple_predicate('>=').
simple_predicate('=<').
simple_predicate('\\+').

% Original partitioning function for backward compatibility
partition_clauses([], _, [], []).
partition_clauses([Clause|Clauses], Name/Arity, [Clause|BaseCases], RecCases) :-
    is_base_case(Clause, Name/Arity), !,
    partition_clauses(Clauses, Name/Arity, BaseCases, RecCases).
partition_clauses([Clause|Clauses], Name/Arity, BaseCases, [Clause|RecCases]) :-
    partition_clauses(Clauses, Name/Arity, BaseCases, RecCases).

% Check if a clause is a base case (direct recursion check only)
is_base_case(clause(_, Body), Name/Arity) :-
    \+ contains_recursive_call(Body, Name/Arity).

% Check if body contains direct recursive call
contains_recursive_call(true, _) :- !, fail.
contains_recursive_call((A, B), Name/Arity) :-
    contains_recursive_call(A, Name/Arity); 
    contains_recursive_call(B, Name/Arity).
contains_recursive_call(Goal, Name/Arity) :-
    functor(Goal, Name, Arity).

% Check if body contains indirect recursive call (through helper predicates)
contains_indirect_recursion(_, _, _) :- false.  % Placeholder

% Validate base cases
validate_base_cases(BaseCases, valid) :- BaseCases \= [], !.
validate_base_cases(_, invalid).

% Validate inductive steps
validate_inductive_steps([], _, valid) :- !. % No recursive cases is valid
validate_inductive_steps(RecCases, Name/Arity, valid) :-
    % Each recursive call should be to structurally smaller argument
    forall(member(clause(Head, Body), RecCases),
           valid_inductive_step(Head, Body, Name/Arity)), !.
validate_inductive_steps(_, _, invalid).

% Check if an inductive step is valid
valid_inductive_step(Head, Body, Name/Arity) :-
    find_recursive_calls(Body, Name/Arity, Calls),
    forall(member(Call, Calls), structurally_smaller(Call, Head)).

% Find all recursive calls in a body
find_recursive_calls(true, _, []) :- !.
find_recursive_calls((A, B), Name/Arity, Calls) :-
    !,
    find_recursive_calls(A, Name/Arity, CallsA),
    find_recursive_calls(B, Name/Arity, CallsB),
    append(CallsA, CallsB, Calls).
find_recursive_calls(Goal, Name/Arity, [Goal]) :-
    functor(Goal, GName, GArity),
    GName == Name, GArity == Arity, !.
find_recursive_calls(_, _, []).

% Check if recursive argument is structurally smaller
structurally_smaller(RecCall, Head) :-
    functor(RecCall, Name, Arity),
    functor(Head, Name, Arity),
    recursive_structure_type(Head, Type),
    recursive_argument_index(Type, Index),
    arg(Index, Head, HeadArg),
    arg(Index, RecCall, CallArg),
    is_structurally_smaller(CallArg, HeadArg, Type).

% Determine the recursive structure type
recursive_structure_type(Term, list) :-
    arg(1, Term, Arg),
    nonvar(Arg),
    Arg = [_|_], !.
recursive_structure_type(Term, number) :-
    arg(1, Term, Arg),
    integer(Arg), !.
recursive_structure_type(_, tree). % Default assumption

% Get index of recursive argument
recursive_argument_index(list, 1).    % Assume first arg for lists
recursive_argument_index(number, 1).  % Assume first arg for numbers
recursive_argument_index(tree, 1).    % Assume first arg for trees

% Check if term is structurally smaller
is_structurally_smaller([], [_|_], list) :- !.
is_structurally_smaller([_|T1], [_|T2], list) :-
    length(T1, L1),
    length(T2, L2),
    L1 < L2, !.
is_structurally_smaller(N1, N2, number) :-
    integer(N1), integer(N2),
    N1 < N2, !.
is_structurally_smaller(empty, tree(_,_,_), tree) :- !.
is_structurally_smaller(tree(_, Left, _), tree(_, _, _), tree) :-
    Left \= empty, !.
is_structurally_smaller(tree(_, _, Right), tree(_, _, _), tree) :-
    Right \= empty, !.

% Check tail recursion
check_tail_recursion(RecCases, Name/Arity, valid) :-
    forall(member(clause(_, Body), RecCases),
           is_tail_recursive_call(Body, Name/Arity)), !.
check_tail_recursion(_, _, invalid).

% Check if body has tail-recursive call
is_tail_recursive_call(true, _) :- !, fail.
is_tail_recursive_call((A, B), Name/Arity) :- !,
    \+ contains_recursive_call(A, Name/Arity),
    is_tail_recursive_call(B, Name/Arity).
is_tail_recursive_call(Goal, Name/Arity) :-
    functor(Goal, GName, GArity),
    GName == Name, GArity == Arity.

% Generate report
generate_report(Results, Options) :-
    format('~n==== Prolog Inductive Debugger Report ====~n~n', []),
    print_all_results(Results, Options),
    format('~n==== End of Report ====~n~n', []).

% Print all results
print_all_results([], _).
print_all_results([Result|Results], Options) :-
    print_result(Result, Options),
    print_all_results(Results, Options).

% Print a single result
print_result(result(Name/Arity, Analysis), Options) :-
    format('Predicate: ~w/~d~n', [Name, Arity]),
    format('-------------------~n', []),
    
    % Print each check result
    member(base_case(BaseCaseResult), Analysis),
    print_check('Base Case', BaseCaseResult),
    
    member(inductive_step(InductiveResult), Analysis),
    print_check('Inductive Step', InductiveResult),
    
    member(termination(TerminationResult), Analysis),
    print_check('Termination', TerminationResult),
    
    member(tail_recursion(TailRecResult), Analysis),
    print_tail_recursion_result(TailRecResult),
    
    % Print bugs if any
    member(bugs(Bugs), Analysis),
    print_bugs(Bugs),
    
    % Print proof trace if requested and available
    member(proof_trace_result(ProofTraceResult), Analysis),
    ((ProofTraceResult \= none, member(verbose, Options)) -> 
        print_proof_trace(ProofTraceResult)
    ; (ProofTraceResult \= none, member(generate_proof, Options)) ->
        format('Proof trace generated (use --verbose to display)~n')
    ;
        true
    ),
    
    format('~n', []).

% Print check result
print_check(Name, valid) :-
    format('✓ ~w: valid~n', [Name]).
print_check(Name, invalid) :-
    format('✗ ~w: invalid~n', [Name]).

% Print tail recursion result
print_tail_recursion_result(not_checked) :-
    format('- Tail Recursion: not checked~n', []).
print_tail_recursion_result(Result) :-
    print_check('Tail Recursion', Result).

% Print bugs
print_bugs([]) :-
    format('✓ All checks passed.~n', []).
print_bugs(Bugs) :-
    format('✗ Issues found: ~w~n', [Bugs]).

% Print proof trace
print_proof_trace(proof_trace(ProofTrace)) :-
    ProofTrace = proof_trace(Name/Arity, BaseProofs, IndProofs),
    format('~nProof Trace:~n', []),
    format('  Predicate: ~w/~d~n', [Name, Arity]),
    
    format('  Base Cases:~n'),
    forall(member(K-base_proof(Head, Body, _), BaseProofs),
           format('    ~w: ~w :- ~w~n', [K, Head, Body])),
    
    format('  Inductive Steps:~n'),
    forall(member(K-inductive_proof(Head, Body, Unfolding), IndProofs),
          (format('    ~w: ~w :- ~w~n', [K, Head, Body]),
           format('      Unfolds to: ~w~n', [Unfolding]))).