% Example: Tree traversal (inorder)
% Tree representation: empty or tree(Value, LeftSubtree, RightSubtree)
inorder(empty, []).
inorder(tree(V, L, R), Result) :-
inorder(L, LResult),
inorder(R, RResult),
append(LResult, [V|RResult], Result).

% Helper predicate
append([], L, L).
append([H|T], L, [H|R]) :- append(T, L, R).
