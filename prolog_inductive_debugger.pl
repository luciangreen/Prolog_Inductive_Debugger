% Prolog Inductive Debugger (PID)
% Author: luciangreen
% Date: 2025-06-21 20:33:06

% Main entry point
pid(File, Options) :-
    format('Prolog Inductive Debugger (PID)~n'),
    format('Analyzing file: ~w~n~n', [File]),
    
    % Parse the input file
    parse_file(File, Predicates),
    
    % Analyze predicates
    analyze_predicates(Predicates, Options, Results),
    
    % Generate report
    generate_report(Results, Options).

% Parse an input file to get predicate definitions
parse_file(File, Predicates) :-
    (exists_file(File) ->
        load_predicates_from_file(File, Predicates)
    ;
        format('Error: File not found: ~w~n', [File]),
        fail
    ).

% Load predicates from a file
load_predicates_from_file(File, Predicates) :-
    open(File, read, Stream),
    read_all_predicates(Stream, Predicates),
    close(Stream).

% Read all predicates from a stream
read_all_predicates(Stream, Predicates) :-
    read_term(Stream, Term, []),
    (Term == end_of_file ->
        Predicates = []
    ;
        process_term(Term, Predicate),
        Predicates = [Predicate|Rest],
        read_all_predicates(Stream, Rest)
    ).

% Process a term to create a predicate representation
process_term((Head :- Body), predicate(Name/Arity, [clause(Head, Body)])) :- !,
    functor(Head, Name, Arity).
process_term(Fact, predicate(Name/Arity, [clause(Fact, true)])) :-
    functor(Fact, Name, Arity).

% Analyze all predicates
analyze_predicates([], _, []).
analyze_predicates([Predicate|Predicates], Options, [Result|Results]) :-
    analyze_single_predicate(Predicate, Options, Result),
    analyze_predicates(Predicates, Options, Results).

% Analyze a single predicate
analyze_single_predicate(predicate(Name/Arity, Clauses), Options, result(Name/Arity, Analysis)) :-
    % Group clauses by name/arity
    group_clauses(Name/Arity, Clauses, AllClauses),
    
    % Extract recursive structure
    extract_structure(AllClauses, Name/Arity, Structure),
    
    % Analyze the structure
    validate_structure(Structure, Options, Analysis).

% Group clauses by name/arity
group_clauses(Name/Arity, Clauses, GroupedClauses) :-
    findall(clause(Head, Body),
            (member(clause(Head, Body), Clauses),
             functor(Head, Name, Arity)),
            GroupedClauses).

% Extract recursive structure from clauses
extract_structure(Clauses, Name/Arity, structure(Name/Arity, BaseCases, RecCases)) :-
    partition_clauses(Clauses, Name/Arity, BaseCases, RecCases).

% Partition clauses into base cases and recursive cases
partition_clauses([], _, [], []).
partition_clauses([clause(Head, Body)|Clauses], Name/Arity, [clause(Head, Body)|BaseCases], RecCases) :-
    \+ contains_recursive_call(Body, Name/Arity), !,
    partition_clauses(Clauses, Name/Arity, BaseCases, RecCases).
partition_clauses([Clause|Clauses], Name/Arity, BaseCases, [Clause|RecCases]) :-
    partition_clauses(Clauses, Name/Arity, BaseCases, RecCases).

% Check if body contains recursive call
contains_recursive_call(true, _) :- !, fail.
contains_recursive_call((A, B), Name/Arity) :-
    contains_recursive_call(A, Name/Arity); 
    contains_recursive_call(B, Name/Arity), !.
contains_recursive_call(Goal, Name/Arity) :-
    functor(Goal, GoalName, GoalArity),
    GoalName = Name,
    GoalArity = Arity.

% Validate the structure of a predicate
validate_structure(Structure, Options, Analysis) :-
    Structure = structure(Name/Arity, BaseCases, RecCases),
    
    % Check base cases
    check_base_cases(BaseCases, BaseCaseValid),
    
    % Check inductive steps
    check_inductive_steps(RecCases, Name/Arity, InductiveStepsValid),
    
    % Check termination
    check_termination(BaseCaseValid, InductiveStepsValid, TerminationValid),
    
    % Check tail recursion if requested
    (member(check_tail_recursion, Options) ->
        check_tail_recursion(RecCases, Name/Arity, TailRecursiveValid)
    ;   
        TailRecursiveValid = not_checked),
    
    % Generate proof trace if requested
    (member(generate_proof, Options) ->
        generate_proof_trace(Structure, ProofTrace),
        ProofTraceResult = [proof_trace(ProofTrace)]
    ;
        ProofTraceResult = []
    ),
    
    % Detect bugs
    detect_bugs(Structure, BaseCaseValid, InductiveStepsValid, TerminationValid, TailRecursiveValid, Bugs),
    
    % Combine results
    Analysis = [
        base_case(BaseCaseValid),
        inductive_step(InductiveStepsValid),
        termination(TerminationValid),
        tail_recursion(TailRecursiveValid),
        bugs(Bugs)
        | ProofTraceResult
    ].

% Check if predicate has valid base cases
check_base_cases(BaseCases, valid) :- BaseCases \= [], !.
check_base_cases(_, invalid).

% Check if inductive steps are valid
check_inductive_steps(RecCases, Name/Arity, valid) :-
    RecCases \= [],
    forall(member(clause(Head, Body), RecCases),
           valid_inductive_step(Head, Body, Name/Arity)), !.
check_inductive_steps(_, _, invalid).

% Check if an inductive step is valid (makes structural progress)
valid_inductive_step(Head, Body, Name/Arity) :-
    find_recursive_calls(Body, Name/Arity, Calls),
    forall(member(Call, Calls), 
           structurally_smaller(Call, Head, Name/Arity)).

% Find all recursive calls in a body
find_recursive_calls(true, _, []) :- !.
find_recursive_calls((A, B), Name/Arity, Calls) :-
    !,
    find_recursive_calls(A, Name/Arity, CallsA),
    find_recursive_calls(B, Name/Arity, CallsB),
    append(CallsA, CallsB, Calls).
find_recursive_calls(Goal, Name/Arity, [Goal]) :-
    functor(Goal, GoalName, GoalArity),
    GoalName = Name,
    GoalArity = Arity, !.
find_recursive_calls(_, _, []).

% Check if a recursive call is structurally smaller
structurally_smaller(Call, Head, Name/Arity) :-
    % Determine the recursive argument
    recursive_argument_index(Name/Arity, Index),
    arg(Index, Head, HeadArg),
    arg(Index, Call, CallArg),
    
    % Check if structurally smaller
    is_structurally_smaller(CallArg, HeadArg).

% Determine recursive argument index (simplified - in full implementation would be more sophisticated)
recursive_argument_index(_Name/_Arity, 1).  % Assume first argument for simplicity

% Check if term is structurally smaller
is_structurally_smaller([], [_|_]) :- !.  % Empty list is smaller than non-empty list
is_structurally_smaller([_|T1], [_|T2]) :-
    is_structurally_smaller(T1, T2), !.  % Recursively check tails
is_structurally_smaller(N1, N2) :-
    number(N1), number(N2),
    N1 < N2, !.  % Smaller number
is_structurally_smaller(empty, tree(_, _, _)) :- !.  % Empty tree is smaller than non-empty
is_structurally_smaller(tree(_, L, _), tree(_, _, _)) :-
    L \= empty, !.  % Left subtree exists, recursive case
is_structurally_smaller(tree(_, _, R), tree(_, _, _)) :-
    R \= empty, !.  % Right subtree exists, recursive case

% Check termination
check_termination(valid, valid, valid) :- !.  % Both base case and inductive step valid
check_termination(_, _, invalid).

% Check tail recursion
check_tail_recursion(RecCases, Name/Arity, valid) :-
    forall(member(clause(_, Body), RecCases),
           is_tail_recursive_call(Body, Name/Arity)), !.
check_tail_recursion(_, _, invalid).

% Check if call is in tail position
is_tail_recursive_call(true, _) :- !, fail.
is_tail_recursive_call((A, B), Name/Arity) :-
    \+ contains_recursive_call(A, Name/Arity),
    is_tail_recursive_call(B, Name/Arity), !.
is_tail_recursive_call(Goal, Name/Arity) :-
    functor(Goal, GoalName, GoalArity),
    GoalName = Name,
    GoalArity = Arity.

% Generate proof trace
generate_proof_trace(Structure, ProofTrace) :-
    Structure = structure(Name/Arity, BaseCases, RecCases),
    
    % Process base cases
    process_base_cases(BaseCases, BaseProofs),
    
    % Process recursive cases
    process_recursive_cases(RecCases, Structure, RecProofs),
    
    % Combine results
    ProofTrace = [
        predicate(Name/Arity),
        base_cases(BaseProofs),
        recursive_cases(RecProofs)
    ].

% Process base cases for proof trace
process_base_cases([], []).
process_base_cases([clause(Head, Body)|Rest], [base_case(Head, Body)|Traces]) :-
    process_base_cases(Rest, Traces).

% Process recursive cases for proof trace
process_recursive_cases([], _, []).
process_recursive_cases([clause(Head, Body)|Rest], Structure, [rec_case(Head, Body, Unfolding)|Traces]) :-
    unfold_body(Body, Structure, Unfolding),
    process_recursive_cases(Rest, Structure, Traces).

% Unfold a body for proof trace
unfold_body(true, _, true) :- !.
unfold_body((A, B), Structure, (UnfoldA, UnfoldB)) :-
    !,
    unfold_body(A, Structure, UnfoldA),
    unfold_body(B, Structure, UnfoldB).
unfold_body(Goal, Structure, unfold(Goal, Unfolded)) :-
    Structure = structure(Name/Arity, _, _),
    functor(Goal, GoalName, GoalArity),
    GoalName = Name,
    GoalArity = Arity,
    !,
    % Find matching clause (simplified)
    Unfolded = recursive_call.  % In full implementation, would actually unfold
unfold_body(Goal, _, Goal).

% Detect bugs
detect_bugs(Structure, BaseCaseValid, InductiveStepsValid, TerminationValid, TailRecursiveValid, Bugs) :-
    detect_base_case_bugs(BaseCaseValid, BaseBugs),
    detect_inductive_step_bugs(Structure, InductiveStepsValid, IndBugs),
    detect_termination_bugs(TerminationValid, TermBugs),
    detect_tail_recursion_bugs(TailRecursiveValid, TailBugs),
    
    % Combine bugs
    append_all([BaseBugs, IndBugs, TermBugs, TailBugs], Bugs).

% Helper to append multiple lists
append_all([], []).
append_all([List|Lists], Result) :-
    append_all(Lists, Rest),
    append(List, Rest, Result).

% Detect base case bugs
detect_base_case_bugs(valid, []) :- !.
detect_base_case_bugs(invalid, [missing_base_case]).

% Detect inductive step bugs
detect_inductive_step_bugs(_, valid, []) :- !.
detect_inductive_step_bugs(_, invalid, [incorrect_recursion_pattern]).

% Detect termination bugs
detect_termination_bugs(valid, []) :- !.
detect_termination_bugs(invalid, [non_terminating]).

% Detect tail recursion bugs
detect_tail_recursion_bugs(valid, []) :- !.
detect_tail_recursion_bugs(not_checked, []) :- !.
detect_tail_recursion_bugs(invalid, [non_tail_recursive]).

% Generate report
generate_report(Results, Options) :-
    format('==== Prolog Inductive Debugger Report ====~n~n', []),
    member(verbose, Options) -> VerboseMode = true ; VerboseMode = false,
    forall(member(Result, Results), print_result(Result, VerboseMode)),
    format('==== End of Report ====~n', []).

% Print a single result
print_result(result(Name/Arity, Analysis), Verbose) :-
    format('Predicate: ~w/~d~n', [Name, Arity]),
    format('-------------------~n', []),
    
    % Extract analysis results
    member(base_case(BaseCaseResult), Analysis),
    member(inductive_step(InductiveResult), Analysis),
    member(termination(TerminationResult), Analysis),
    member(tail_recursion(TailRecursiveResult), Analysis),
    member(bugs(Bugs), Analysis),
    
    % Print results
    print_check('Base Case', BaseCaseResult),
    print_check('Inductive Step', InductiveResult),
    print_check('Termination', TerminationResult),
    
    (TailRecursiveResult = not_checked ->
        format('- Tail Recursion: not checked~n', [])
    ;
        print_check('Tail Recursion', TailRecursiveResult)
    ),
    
    % Print bugs
    (Bugs = [] ->
        format('✓ All checks passed.~n', [])
    ;
        format('✗ Issues found: ~w~n', [Bugs])
    ),
    
    % Print proof trace if available and in verbose mode
    (Verbose, member(proof_trace(ProofTrace), Analysis) ->
        format('~nProof Trace:~n', []),
        print_proof_trace(ProofTrace)
    ;
        true
    ),
    
    format('~n', []).

% Print a check result
print_check(Name, valid) :-
    format('✓ ~w: valid~n', [Name]).
print_check(Name, invalid) :-
    format('✗ ~w: invalid~n', [Name]).

% Print a proof trace
print_proof_trace(ProofTrace) :-
    member(predicate(Name/Arity), ProofTrace),
    format('  Predicate: ~w/~d~n', [Name, Arity]),
    
    member(base_cases(BaseCases), ProofTrace),
    format('  Base Cases:~n', []),
    forall(member(base_case(Head, Body), BaseCases),
           format('    ~w :- ~w~n', [Head, Body])),
    
    member(recursive_cases(RecCases), ProofTrace),
    format('  Recursive Cases:~n', []),
    forall(member(rec_case(Head, Body, Unfolding), RecCases),
          (format('    ~w :- ~w~n', [Head, Body]),
           format('      Unfolds to: ~w~n', [Unfolding]))).

% Command line interface
main([]) :-
    format('Usage: prolog_inductive_debugger.pl <file.pl> [options]~n', []),
    format('Options:~n', []),
    format('  --tail    Check for tail recursion~n', []),
    format('  --proof   Generate proof traces~n', []),
    format('  --verbose Verbose output~n', []),
    halt(1).

main([File|Args]) :-
    parse_args(Args, Options),
    (pid(File, Options) -> 
        halt(0)
    ; 
        halt(1)
    ).

parse_args([], []).
parse_args(['--tail'|Args], [check_tail_recursion|Options]) :- !,
    parse_args(Args, Options).
parse_args(['--proof'|Args], [generate_proof|Options]) :- !,
    parse_args(Args, Options).
parse_args(['--verbose'|Args], [verbose|Options]) :- !,
    parse_args(Args, Options).
parse_args([_|Args], Options) :-
    parse_args(Args, Options).

% Entry point for command-line usage
:- initialization(main, main).