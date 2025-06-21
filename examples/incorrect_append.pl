% Example: Incorrect append (missing base case)
incorrect_append([H|T], L, [H|R]) :- incorrect_append(T, L, R).
