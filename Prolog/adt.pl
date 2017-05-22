% natural numbers: z = 0, s(X): succesor
inc(X, s(X)).

% A1: x + s(y) = s(x + y)
add(X, z, X) :- !.
add(X, s(Y), s(Res)) :- add(X, Y, Res).

inc2(X, Res) :- add(X, s(z), Res).

% A2: x * s(y) = x * y + x
times(X, s(z), X) :- !.
times(X, s(Y), Res) :- times(X, Y, R1), add(R1, X, Res).

to_nat(z, 0).
to_nat(s(X), R) :- to_nat(X, R1), R is R1 + 1.

to_adt(0, z).
to_adt(N, s(X)) :- N1 is N - 1, to_adt(N1, X), !.

test(N) :- to_adt(N, A), to_nat(A, N).

% lists

% rev(e:l) = rev(l) ++ (e:[])
rev([], []).
rev([H|T], Rev) :- rev(T, R1), append(R1, [H], Rev).
