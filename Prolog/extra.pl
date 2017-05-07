
cons(X, L, [X|L]).
swap_cons(L, X, [X|L]).

rev(L, R) :- rev_aux(L, R, []).
rev_aux([], L, L). % Out = Acc
rev_aux([H|T], X, Acc) :- rev_aux(T, X, [H|Acc]).


inc(X, O) :- O is X + 1.

map(_, [], []) :- !.
map(F, [H|T], [O|Rest]) :- call(F, H, O), map(F, T, Rest).


foldl(_, Acc, [], Acc) :- !.
foldl(Op, Acc, [H|T], Out) :- call(Op, Acc, H, NewAcc), % Op(Acc, H, NewAcc)
						foldl(Op, NewAcc, T, Out).

foldr(_, Acc, [], Acc).
%% foldr(Op, Acc, [H|T], Out) :- foldr(Op, Acc, T, Rest), call(Op, H, Rest) 


frev(L, R) :- foldl(swap_cons, [], L, R).

is_palindrome(L) :- frev(L, L).
