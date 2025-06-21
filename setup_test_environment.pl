% Setup test environment for Prolog Inductive Debugger

% Create directories if they don't exist
:- (exists_directory('examples') -> true ; make_directory('examples')).

% Define setup examples predicate
setup_examples :-
    write_example_file(
        'examples/reverse.pl',
        "% Example: List reversal with accumulator (tail recursive)\n\
reverse(L, R) :- reverse_acc(L, [], R).\n\
\n\
reverse_acc([], Acc, Acc).\n\
reverse_acc([H|T], Acc, R) :- reverse_acc(T, [H|Acc], R).\n"
    ),

    write_example_file(
        'examples/reverse_naive.pl',
        "% Example: List reversal without accumulator (not tail recursive)\n\
reverse_naive([], []).\n\
reverse_naive([H|T], R) :-\n\
    reverse_naive(T, RT),\n\
    append(RT, [H], R).\n\
\n\
% Helper predicate\n\
append([], L, L).\n\
append([H|T], L, [H|R]) :- append(T, L, R).\n"
    ),

    write_example_file(
        'examples/factorial.pl',
        "% Example: Factorial calculation\n\
factorial(0, 1).\n\
factorial(N, F) :-\n\
    N > 0,\n\
    N1 is N - 1,\n\
    factorial(N1, F1),\n\
    F is N * F1.\n"
    ),

    write_example_file(
        'examples/incorrect_append.pl',
        "% Example: Incorrect append (missing base case)\n\
incorrect_append([H|T], L, [H|R]) :- incorrect_append(T, L, R).\n"
    ),
    
    write_example_file(
        'examples/fibonacci_bad.pl',
        "% Example: Fibonacci with bad base case\n\
fibonacci_bad(0, 1).\n\
% Missing fibonacci_bad(1, 1) base case!\n\
fibonacci_bad(N, F) :-\n\
    N > 1,\n\
    N1 is N - 1,\n\
    N2 is N - 2,\n\
    fibonacci_bad(N1, F1),\n\
    fibonacci_bad(N2, F2),\n\
    F is F1 + F2.\n"
    ),
    
    write_example_file(
        'examples/infinite_recursion.pl',
        "% Example: Infinite recursion (no structural progress)\n\
count_forever(X) :- count_forever(X).\n"
    ),
    
    write_example_file(
        'examples/tree_traversal.pl',
        "% Example: Tree traversal (inorder)\n\
% Tree representation: empty or tree(Value, LeftSubtree, RightSubtree)\n\
inorder(empty, []).\n\
inorder(tree(V, L, R), Result) :-\n\
    inorder(L, LResult),\n\
    inorder(R, RResult),\n\
    append(LResult, [V|RResult], Result).\n\
\n\
% Helper predicate\n\
append([], L, L).\n\
append([H|T], L, [H|R]) :- append(T, L, R).\n"
    ),
    
    write_example_file(
        'examples/tree_sum.pl',
        "% Example: Tree sum (non-tail-recursive)\n\
% Tree representation: empty or tree(Value, LeftSubtree, RightSubtree)\n\
tree_sum(empty, 0).\n\
tree_sum(tree(V, L, R), Sum) :-\n\
    tree_sum(L, LSum),\n\
    tree_sum(R, RSum),\n\
    Sum is V + LSum + RSum.\n"
    ),
    
    write_example_file(
        'examples/tree_sum_tail.pl',
        "% Example: Tree sum (tail-recursive)\n\
% Tree representation: empty or tree(Value, LeftSubtree, RightSubtree)\n\
tree_sum(Tree, Sum) :- tree_sum_acc(Tree, 0, Sum).\n\
\n\
tree_sum_acc(empty, Acc, Acc).\n\
tree_sum_acc(tree(V, L, R), Acc, Sum) :-\n\
    Acc1 is Acc + V,\n\
    tree_sum_acc(L, Acc1, Sum1),\n\
    tree_sum_acc(R, Sum1, Sum).\n"
    ),
    
    write_example_file(
        'examples/incorrect_accumulator.pl',
        "% Example: Incorrect accumulator use\n\
sum_list_bad(List, Sum) :- sum_list_bad_acc(List, 0, Sum).\n\
\n\
sum_list_bad_acc([], Sum, Sum).\n\
sum_list_bad_acc([H|T], Acc, Sum) :-\n\
    % Bug: Should be NewAcc is Acc + H\n\
    sum_list_bad_acc(T, Acc, NewSum),\n\
    NewSum is Acc + H.\n"
    ).

% Helper to write example files
write_example_file(Filename, Content) :-
    open(Filename, write, Stream),
    write(Stream, Content),
    close(Stream).

% Now call setup_examples after it's defined
:- setup_examples.