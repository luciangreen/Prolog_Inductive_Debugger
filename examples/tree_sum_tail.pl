% Example: Tree sum (tail-recursive)
% Tree representation: empty or tree(Value, LeftSubtree, RightSubtree)
tree_sum(Tree, Sum) :- tree_sum_acc(Tree, 0, Sum).

tree_sum_acc(empty, Acc, Acc).
tree_sum_acc(tree(V, L, R), Acc, Sum) :-
Acc1 is Acc + V,
tree_sum_acc(L, Acc1, Sum1),
tree_sum_acc(R, Sum1, Sum).
