triangulo(a, lados(3,4,5)).
triangulo(b, lados(3,25,26)).

perimetro(X,P) :- triangulo(X, lados(A,B,C)), P is (A+B+C).
semiperimeter(A,B,C,S) :- integer(A), integer(B), integer(C), S is (A+B+C)/2.
area(X,P) :- triangulo(X, lados(A,B,C)), semiperimeter(A,B,C,S), P is sqrt(S*(S-A)*(S-B)*(S-C)).
