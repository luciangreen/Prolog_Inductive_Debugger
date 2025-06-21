% Bug Detector for PID
% Author: luciangreen (lg3)
% Date: 2025-06-21 21:48:09
:- module(bug_detector, [detect_bugs/6]).

% Detect bugs in a predicate definition
detect_bugs(Structure, BaseCaseValid, InductiveStepsValid, TerminationValid, TailRecursiveValid, Bugs) :-
    detect_base_case_bugs(BaseCaseValid, BaseBugs),
    detect_inductive_step_bugs(InductiveStepsValid, IndBugs),
    detect_termination_bugs(TerminationValid, TermBugs),
    detect_tail_recursion_bugs(TailRecursiveValid, TailBugs),
    detect_structural_bugs(Structure, StructBugs),
    
    % Combine all bugs
    append_all_bugs([BaseBugs, IndBugs, TermBugs, TailBugs, StructBugs], Bugs).

% Helper to append multiple lists of bugs
append_all_bugs([], []).
append_all_bugs([List|Lists], Result) :-
    append_all_bugs(Lists, Rest),
    append(List, Rest, Result).

% Detect base case bugs
detect_base_case_bugs(valid, []) :- !.
detect_base_case_bugs(invalid, [missing_base_case]).

% Detect inductive step bugs
detect_inductive_step_bugs(valid, []) :- !.
detect_inductive_step_bugs(invalid, [incorrect_recursion_pattern]).

% Detect termination bugs
detect_termination_bugs(valid, []) :- !.
detect_termination_bugs(invalid, [non_terminating]).

% Detect tail recursion bugs
detect_tail_recursion_bugs(valid, []) :- !.
detect_tail_recursion_bugs(not_checked, []) :- !.
detect_tail_recursion_bugs(invalid, [non_tail_recursive]).

% Detect structural bugs in a predicate (simplified)
detect_structural_bugs(_Structure, []).