foo(X,Y) :- fee(X, Y-[]).

fee([], X-X).
fee([X|Y], Z-W) :- fee(Y,Z-[X|W]).
