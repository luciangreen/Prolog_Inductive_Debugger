% Example: Tree sum (non-tail-recursive)
% Tree representation: empty or tree(Value, LeftSubtree, RightSubtree)
tree_sum(empty, 0).
tree_sum(tree(V, L, R), Sum) :-
tree_sum(L, LSum),
tree_sum(R, RSum),
Sum is V + LSum + RSum.
