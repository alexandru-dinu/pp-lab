% knowledge bases and simple interogations
father(george, ana).
father(george, mike).
siblings(X, Y) :- father(F, X), father(F, Y).


% list operations
% in Prolog, list are not homogenous
% we can have [1, 2, [3, 4, 5], [6, [7]], 8, 9, [10]]
% programming relies upon pattern matching and goal satisfaction

% in:list
% out:S
lsize([], 0). % size of the empty list is 0
lsize([_|T], S) :- lsize(T, Snext), S is Snext + 1.

% in:list
% out:sum
lsum([], 0).
lsum([H|T], S) :- lsum(T, Snext), S is H + Snext.

% in:list
% out:H
lhead([H|_], H).
% head([], Out) is false 

% in:list
% out:T
ltail([_|T], T).
% list([], Out) is false

% in:n, list
% out:first n elems from list
ltake(_, [], []). % empty list case
ltake(N, _, []) :- N = 0, !.
%ltake(N, L, L) :- lsize(L, N), !.
ltake(N, [H|T], [H|Rest]) :- N1 is N - 1, ltake(N1, T, Rest), !.