% Example: Incorrect accumulator use
sum_list_bad(List, Sum) :- sum_list_bad_acc(List, 0, Sum).

sum_list_bad_acc([], Sum, Sum).
sum_list_bad_acc([H|T], Acc, Sum) :-
% Bug: Should be NewAcc is Acc + H
sum_list_bad_acc(T, Acc, NewSum),
NewSum is Acc + H.
