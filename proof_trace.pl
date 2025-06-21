% Proof Trace Generator for PID
:- module(proof_trace, [generate_proof_trace/2]).

% Generate proof trace for a predicate
generate_proof_trace(Structure, ProofTrace) :-
    Structure = structure(Name/Arity, BaseCases, RecCases),
    
    % Create base case proofs
    map_list_to_pairs(create_base_proof, BaseCases, BaseProofs),
    
    % Create inductive step proofs
    map_list_to_pairs(create_inductive_proof(Structure), RecCases, InductiveProofs),
    
    % Combine into full proof trace
    ProofTrace = proof_trace(Name/Arity, BaseProofs, InductiveProofs).

% Create proof for a base case
create_base_proof(pred(Head, Body), base_proof(Head, Body, true)).

% Create proof for an inductive step
create_inductive_proof(Structure, pred(Head, Body), inductive_proof(Head, Body, Unfolding)) :-
    unfold_recursive_calls(Body, Structure, Unfolding).

% Unfold recursive calls in the body
unfold_recursive_calls(true, _, []) :- !.
unfold_recursive_calls((A, B), Structure, [UnfoldA|UnfoldB]) :-
    !,
    unfold_recursive_calls(A, Structure, UnfoldA),
    unfold_recursive_calls(B, Structure, UnfoldB).
unfold_recursive_calls(Goal, Structure, [unfold(Goal, Unfolded)]) :-
    Structure = structure(Name/Arity, _, _),
    functor(Goal, Name, Arity),
    !,
    % Find matching clause
    apply_matching_clause(Goal, Structure, Unfolded).
unfold_recursive_calls(Goal, _, [builtin(Goal)]).

% Apply a matching clause to unfold a recursive call
apply_matching_clause(Goal, Structure, Unfolded) :-
    Structure = structure(_, BaseCases, RecCases),
    append(BaseCases, RecCases, AllClauses),
    member(pred(Head, Body), AllClauses),
    Head =.. HeadList,
    Goal =.. GoalList,
    (   unifiable(HeadList, GoalList, Unifier)
    ->  apply_unifier(Body, Unifier, Unfolded)
    ;   Unfolded = failed_to_match
    ).

% Apply unifier to a term
apply_unifier(Term, Unifier, Result) :-
    copy_term(Term-Unifier, TermCopy-UnifierCopy),
    apply_substitutions(TermCopy, UnifierCopy),
    Result = TermCopy.

% Apply substitutions from unifier
apply_substitutions(_, []).
apply_substitutions(Term, [Var=Val|Rest]) :-
    subst_var(Term, Var, Val),
    apply_substitutions(Term, Rest).

% Substitute a variable with a value in a term
subst_var(Term, Var, Val) :-
    term_variables(Term, Vars),
    member(Var, Vars),
    Var = Val.

% Print a readable proof trace
print_proof_trace(proof_trace(Name/Arity, BaseProofs, InductiveProofs)) :-
    format('Proof trace for ~w/~d:~n~n', [Name, Arity]),
    
    % Print base case proofs
    format('Base Cases:~n', []),
    forall(member(base_proof(Head, Body, _), BaseProofs),
           format('  ~w :- ~w~n', [Head, Body])),
    
    % Print inductive step proofs
    format('~nInductive Steps:~n', []),
    forall(member(inductive_proof(Head, Body, Unfolding), InductiveProofs),
           (format('  ~w :- ~w~n', [Head, Body]),
            format('    Unfolds to: ~w~n', [Unfolding]))),
    
    format('~n', []).