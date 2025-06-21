% Example: Infinite recursion (no structural progress)
count_forever(X) :- count_forever(X).
