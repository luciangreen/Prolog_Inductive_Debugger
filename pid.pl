% Prolog Inductive Debugger (PID)
% Author: luciangreen
% Date: 2025-06-21

% Main entry point
pid_main(File, Options) :-
    parse_file(File, Predicates),
    analyze_predicates(Predicates, Options, Results),
    generate_report(Results, Options).

% Parse input file or text
parse_file(File, Predicates) :-
    (is_file(File) -> 
        read_file_to_terms(File, Terms, [])
    ;   
        read_term_from_atom(File, Terms, [])),
    extract_predicates(Terms, Predicates).

% Extract predicates from terms
extract_predicates([], []).
extract_predicates([Term|Terms], [Predicate|Predicates]) :-
    (Term = (Head :- Body)) ->
        Predicate = pred(Head, Body)
    ;   % Handle facts
        Predicate = pred(Term, true),
    extract_predicates(Terms, Predicates).

% Analyze predicates
analyze_predicates([], _, []).
analyze_predicates([Predicate|Predicates], Options, [Result|Results]) :-
    analyze_single_predicate(Predicate, Options, Result),
    analyze_predicates(Predicates, Options, Results).

% Analyze a single predicate
analyze_single_predicate(pred(Head, Body), Options, Result) :-
    functor(Head, Name, Arity),
    extract_structure(pred(Head, Body), Structure),
    validate_base_cases(Structure, BaseCaseValid),
    validate_inductive_steps(Structure, InductiveStepsValid),
    check_termination(Structure, TerminationValid),
    (member(check_tail_recursion, Options) ->
        check_tail_recursion(Structure, TailRecursiveValid)
    ;   
        TailRecursiveValid = not_checked),
    detect_bugs(Structure, BaseCaseValid, InductiveStepsValid, TerminationValid, TailRecursiveValid, Bugs),
    
    % Compile results
    Result = result(Name/Arity, 
                   [base_case(BaseCaseValid),
                    inductive_step(InductiveStepsValid),
                    termination(TerminationValid),
                    tail_recursion(TailRecursiveValid),
                    bugs(Bugs)]).

% Extract recursive structure
extract_structure(Predicate, Structure) :-
    % Identify predicate name and arity
    Predicate = pred(Head, _),
    functor(Head, Name, Arity),
    
    % Find all clauses for this predicate
    findall(pred(ClauseHead, ClauseBody),
           (pred(ClauseHead, ClauseBody),
            functor(ClauseHead, Name, Arity)),
           Clauses),
    
    % Classify clauses as base cases or recursive cases
    partition_clauses(Clauses, Name/Arity, BaseCases, RecursiveCases),
    
    % Build structure representation
    Structure = structure(Name/Arity, BaseCases, RecursiveCases).

% Partition clauses into base cases and recursive cases
partition_clauses([], _, [], []).
partition_clauses([Clause|Clauses], Functor, [Clause|BaseCases], RecursiveCases) :-
    is_base_case(Clause, Functor), !,
    partition_clauses(Clauses, Functor, BaseCases, RecursiveCases).
partition_clauses([Clause|Clauses], Functor, BaseCases, [Clause|RecursiveCases]) :-
    partition_clauses(Clauses, Functor, BaseCases, RecursiveCases).

% Check if clause is a base case (no recursive calls)
is_base_case(pred(_, Body), Name/Arity) :-
    \+ contains_recursive_call(Body, Name/Arity).

% Check if body contains recursive call
contains_recursive_call(true, _) :- !, fail.
contains_recursive_call((A, B), Functor) :-
    contains_recursive_call(A, Functor); 
    contains_recursive_call(B, Functor).
contains_recursive_call(Goal, Name/Arity) :-
    functor(Goal, Name, Arity).

% Validate base cases
validate_base_cases(structure(_, BaseCases, _), valid) :-
    BaseCases \= [], !.
validate_base_cases(_, invalid).

% Validate inductive steps
validate_inductive_steps(structure(_, _, RecursiveCases), valid) :-
    RecursiveCases \= [],
    forall(member(Clause, RecursiveCases), 
           valid_inductive_step(Clause)), !.
validate_inductive_steps(_, invalid).

% Check if inductive step is valid
valid_inductive_step(pred(Head, Body)) :-
    % For each recursive call, check if recursive argument is structurally smaller
    functor(Head, Name, Arity),
    find_recursive_calls(Body, Name/Arity, Calls),
    forall(member(Call, Calls), structurally_smaller(Call, Head)).

% Find all recursive calls in the body
find_recursive_calls(true, _, []) :- !.
find_recursive_calls((A, B), Functor, Calls) :- !,
    find_recursive_calls(A, Functor, CallsA),
    find_recursive_calls(B, Functor, CallsB),
    append(CallsA, CallsB, Calls).
find_recursive_calls(Goal, Name/Arity, [Goal]) :-
    functor(Goal, Name, Arity), !.
find_recursive_calls(_, _, []).

% Check termination
check_termination(Structure, valid) :-
    validate_base_cases(Structure, valid),
    validate_inductive_steps(Structure, valid), !.
check_termination(_, invalid).

% Check if recursive argument is structurally smaller
structurally_smaller(RecCall, Head) :-
    functor(RecCall, Name, Arity),
    functor(Head, Name, Arity),
    recursive_structure_type(Head, Type),
    recursive_argument_index(Type, Index),
    arg(Index, Head, HeadArg),
    arg(Index, RecCall, CallArg),
    is_structurally_smaller(CallArg, HeadArg, Type).

% Determine the recursive structure type (list, tree, number)
recursive_structure_type(Term, list) :-
    arg(1, Term, Arg),
    nonvar(Arg),
    Arg = [_|_], !.
recursive_structure_type(Term, number) :-
    arg(1, Term, Arg),
    integer(Arg), !.
recursive_structure_type(_, tree). % Default assumption

% Get the index of the recursive argument based on structure type
recursive_argument_index(list, 1).    % Assume first arg for lists
recursive_argument_index(number, 1).  % Assume first arg for numbers
recursive_argument_index(tree, 1).    % Assume first arg for trees

% Check if one term is structurally smaller than another
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

% Check if predicate is tail recursive
check_tail_recursion(structure(Name/Arity, _, RecCases), valid) :-
    forall(member(Clause, RecCases),
           tail_recursive_clause(Clause, Name/Arity)), !.
check_tail_recursion(_, invalid).

% Check if a clause is tail recursive
tail_recursive_clause(pred(_, Body), Name/Arity) :-
    normalize_body(Body, NormBody),
    is_tail_recursive(NormBody, Name/Arity).

% Normalize body to handle different syntactic forms
normalize_body((A, B), (A, NormB)) :- !,
    normalize_body(B, NormB).
normalize_body(Term, Term).

% Check if body is tail recursive
is_tail_recursive((A, B), Name/Arity) :- !,
    \+ contains_recursive_call(A, Name/Arity),
    is_tail_recursive(B, Name/Arity).
is_tail_recursive(Goal, Name/Arity) :-
    functor(Goal, Name, Arity).

% Detect bugs
detect_bugs(_, invalid, _, _, _, [missing_base_case]) :- !.
detect_bugs(_, _, invalid, _, _, [incorrect_recursion_pattern]) :- !.
detect_bugs(_, _, _, invalid, _, [non_terminating]) :- !.
detect_bugs(_, _, _, _, invalid, [non_tail_recursive]) :- !.
detect_bugs(_, _, _, _, not_checked, []) :- !.
detect_bugs(_, _, _, _, _, []).

% Generate readable report
generate_report(Results, Options) :-
    format('~n~n==== Prolog Inductive Debugger Report ====~n~n', []),
    forall(member(Result, Results), print_result(Result, Options)),
    format('~n==== End of Report ====~n~n', []).

% Print a single result
print_result(result(Name/Arity, Checks), _) :-
    format('Predicate: ~w/~d~n', [Name, Arity]),
    format('-------------------~n', []),
    
    % Print each check result
    member(base_case(BaseCaseResult), Checks),
    format('✓ Base Case: ~w~n', [BaseCaseResult]),
    
    member(inductive_step(InductiveResult), Checks),
    format('✓ Inductive Step: ~w~n', [InductiveResult]),
    
    member(termination(TerminationResult), Checks),
    format('✓ Termination: ~w~n', [TerminationResult]),
    
    member(tail_recursion(TailRecResult), Checks),
    (TailRecResult = not_checked ->
        format('- Tail Recursion: not checked~n', [])
    ;   
        format('✓ Tail Recursion: ~w~n', [TailRecResult])),
    
    % Print bugs if any
    member(bugs(Bugs), Checks),
    (Bugs = [] -> 
        format('✓ All checks passed.~n', [])
    ;   
        format('✗ Issues found: ~w~n', [Bugs])),
    
    format('~n', []).

% Helper to check if a term is a file
is_file(File) :-
    string(File),
    exists_file(File).

% Command line interface
:- dynamic options/1.
options([]).

main([File|Args]) :-
    parse_args(Args, Options),
    pid_main(File, Options).

parse_args([], []).
parse_args(['--check-tail'|Args], [check_tail_recursion|Options]) :- !,
    parse_args(Args, Options).
parse_args(['--verbose'|Args], [verbose|Options]) :- !,
    parse_args(Args, Options).
parse_args([_|Args], Options) :- 
    parse_args(Args, Options).

% Example usage
% ?- pid_main('reverse.pl', [check_tail_recursion]).