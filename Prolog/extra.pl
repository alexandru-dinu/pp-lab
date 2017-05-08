
cons(X, L, [X|L]).
swap_cons(L, X, [X|L]).

rev(L, R) :- rev_aux(L, R, []).
rev_aux([], L, L). % Out = Acc
rev_aux([H|T], X, Acc) :- rev_aux(T, X, [H|Acc]).


inc(X, O) :- O is X + 1.

minus(A, B, R) :- R is A - B.

even(X) :- 0 is mod(X, 2).

map(_, [], []) :- !.
map(F, [H|T], [O|Rest]) :- call(F, H, O), map(F, T, Rest).

foldl(_, Acc, [], Acc) :- !.
foldl(Op, Acc, [H|T], Out) :- call(Op, Acc, H, NewAcc), foldl(Op, NewAcc, T, Out).

foldr(_, Acc, [], Acc) :- !.
foldr(Op, Acc, [H|T], Out) :- foldr(Op, Acc, T, NewAcc), call(Op, H, NewAcc, Out). 

zipWith(_, [], [], []).
zipWith(_, L, [], L).
zipWith(_, [], L, L).
zipWith(Op, [H1|T1], [H2|T2], [H|Rest]) :- call(Op, H1, H2, H), zipWith(Op, T1, T2, Rest), !.

filter(_, [], []).
filter(Pred, [H|T], [H|Rest]) :- call(Pred, H), filter(Pred, T, Rest), !.
filter(Pred, [_|T], Rest) :- filter(Pred, T, Rest), !.


frev(L, R) :- foldl(swap_cons, [], L, R).

is_palindrome(L) :- frev(L, L).
