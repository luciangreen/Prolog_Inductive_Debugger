% Example: List reversal without accumulator (not tail recursive)
reverse_naive([], []).
reverse_naive([H|T], R) :-
reverse_naive(T, RT),
append(RT, [H], R).

% Helper predicate
append([], L, L).
append([H|T], L, [H|R]) :- append(T, L, R).
