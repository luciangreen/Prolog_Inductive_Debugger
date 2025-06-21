% Example: Fibonacci with bad base case
fibonacci_bad(0, 1).
% Missing fibonacci_bad(1, 1) base case!
fibonacci_bad(N, F) :-
N > 1,
N1 is N - 1,
N2 is N - 2,
fibonacci_bad(N1, F1),
fibonacci_bad(N2, F2),
F is F1 + F2.
