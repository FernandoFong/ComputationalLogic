contiene(X, [H|T]) :- X = H ; contiene(X,T).

binario([H]) :- valida(H).
binario([H|T]) :- valida(H), binario(T).

concatena([],L,L).
concatena([H|T],L2,[H|L3]) :- concatena(T,L2,L3).
 
valida(1).
valida(0).

suma([],0).
suma([H|T], X) :- suma(T,Y) -> X is Y+H.

maximo([X], X).
maximo([H|T], X) :- (X < H -> maximo(T, H) ; maximo(T, X)).

cambia(a, afa).
cambia(e, efe).
cambia(i, ifi).
cambia(o, ofo).
cambia(u, ufu).
cambia(X, X).

transforma([], []).
transforma([H|T],[X|Y]) :- cambia(H,X), transforma(T,Y).
