% Bug Detector for PID
:- module(bug_detector, [detect_bugs/6]).

% Detect bugs in a predicate definition
detect_bugs(Structure, BaseCaseValid, InductiveStepsValid, TerminationValid, TailRecursiveValid, Bugs) :-
    detect_base_case_bugs(Structure, BaseCaseValid, BaseBugs),
    detect_inductive_step_bugs(Structure, InductiveStepsValid, IndBugs),
    detect_termination_bugs(Structure, TerminationValid, TermBugs),
    detect_tail_recursion_bugs(Structure, TailRecursiveValid, TailBugs),
    detect_structural_bugs(Structure, StructBugs),
    
    % Combine all bugs
    append([BaseBugs, IndBugs, TermBugs, TailBugs, StructBugs], Bugs).

% Detect base case bugs
detect_base_case_bugs(_, valid, []) :- !.
detect_base_case_bugs(Structure, invalid, [missing_base_case]) :-
    Structure = structure(_, BaseCases, _),
    BaseCases = [], !.
detect_base_case_bugs(_, invalid, [incorrect_base_case]).

% Detect inductive step bugs
detect_inductive_step_bugs(_, valid, []) :- !.
detect_inductive_step_bugs(Structure, invalid, Bugs) :-
    Structure = structure(Name/Arity, _, RecCases),
    findall(incorrect_recursion_pattern(Clause), 
            (member(Clause, RecCases), 
             \+ valid_inductive_step(Clause, Name/Arity)), 
            Bugs).

% Check if inductive step is valid
valid_inductive_step(pred(Head, Body), Name/Arity) :-
    find_recursive_calls(Body, Name/Arity, Calls),
    forall(member(Call, Calls), structurally_smaller(Call, Head)).

% Detect termination bugs
detect_termination_bugs(_, valid, []) :- !.
detect_termination_bugs(Structure, invalid, [non_terminating]) :-
    Structure = structure(_, BaseCases, RecCases),
    (BaseCases = [] -> true ; 
     find_non_progressing_recursion(Structure, RecCases)).

% Check for non-progressing recursion
find_non_progressing_recursion(Structure, RecCases) :-
    Structure = structure(Name/Arity, _, _),
    member(pred(Head, Body), RecCases),
    find_recursive_calls(Body, Name/Arity, Calls),
    member(Call, Calls),
    \+ structurally_smaller(Call, Head).

% Detect tail recursion bugs
detect_tail_recursion_bugs(_, valid, []) :- !.
detect_tail_recursion_bugs(_, not_checked, []) :- !.
detect_tail_recursion_bugs(Structure, invalid, [non_tail_recursive]) :-
    Structure = structure(Name/Arity, _, _),
    find_non_tail_recursive_calls(Structure, Name/Arity).

% Find non-tail recursive calls
find_non_tail_recursive_calls(Structure, Name/Arity) :-
    Structure = structure(_, _, RecCases),
    member(pred(_, Body), RecCases),
    normalize_body(Body, NormBody),
    \+ is_tail_recursive(NormBody, Name/Arity).

% Detect other structural bugs
detect_structural_bugs(Structure, Bugs) :-
    find_pattern_bugs(Structure, PatternBugs),
    find_accumulator_bugs(Structure, AccBugs),
    append(PatternBugs, AccBugs, Bugs).

% Find pattern matching bugs
find_pattern_bugs(Structure, Bugs) :-
    Structure = structure(_, _, RecCases),
    findall(pattern_matching_issue(Clause),
            (member(Clause, RecCases),
             has_pattern_issue(Clause)),
            Bugs).

% Check for pattern matching issues
has_pattern_issue(pred(Head, _)) :-
    % Find head pattern that might be problematic
    arg(1, Head, Arg),
    nonvar(Arg),
    (   Arg = [_|_],
        % Issue: Not properly decomposing list
        % More specific checks could be added here
        fail
    ;   % Add other pattern issues here
        fail
    ).

% Find accumulator usage bugs
find_accumulator_bugs(Structure, Bugs) :-
    Structure = structure(_, _, RecCases),
    Structure = structure(Name/Arity, _, _),
    uses_accumulator(RecCases, Name/Arity),
    findall(incorrect_accumulator_use(Clause),
            (member(Clause, RecCases),
             incorrect_accumulator_usage(Clause, Name/Arity)),
            Bugs).

% Check if predicate uses accumulators
uses_accumulator(RecCases, _) :-
    member(pred(Head, _), RecCases),
    % Simple heuristic: check if the last argument could be an accumulator
    arg(Arity, Head, LastArg),
    var(LastArg).

% Check for incorrect accumulator usage
incorrect_accumulator_usage(pred(Head, Body), Name/Arity) :-
    find_recursive_calls(Body, Name/Arity, Calls),
    member(Call, Calls),
    % Check if accumulator is used incorrectly
    % This is a simplified check - more sophisticated checks would be needed
    arg(Arity, Head, HeadAcc),
    arg(Arity, Call, CallAcc),
    var(HeadAcc), var(CallAcc),
    HeadAcc \== CallAcc,
    \+ accumulator_transformed(HeadAcc, CallAcc, Body).

% Check if accumulator is properly transformed
accumulator_transformed(HeadAcc, CallAcc, Body) :-
    % Simplified - would need more sophisticated analysis
    contains_term(CallAcc = f(HeadAcc), Body).