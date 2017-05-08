%% INTRO

% key-features of Prolog
% knowledge bases 
% simple interogations
% pattern matching
% goal satisfaction via automatic backtracking

% here we have facts that translate in this way to natural language:
% alex is-a student
student(alex).
% tom is-a student
student(tom).

% alex studies science
studies(alex, science).
% tom studies literature
studies(tom, literature).

% what does alex study? => studies(alex, Out) => Out will unify with science
% who studies literature? => studies(Out, literature) => Out will unify with tom


% a predicate has the following signature: <name>/<arity>

% for example: father/2
% abe's father is orville
father(orville, abe).
father(abe, homer).

father(homer, bart).
father(homer, lisa).
father(homer, maggie).

% interrogate: all kids of homer => ?- parent(homer, K)

% X's grandfather is Y
% X's grandfather is the father of X's father
grandfather(X, Y) :- father(X, Z), father(Z, Y).





% warm-up
palindrome(L) :- reverse(L, L).


%% LIST OPERATIONS
% in Prolog, list are not homogenous
% we can have [1, 2, [3, 4, 5], [6, [7]], 8, 9, [10]]

% fact: size of the empty list is 0
lsize([], 0).
lsize([_|T], S) :- lsize(T, Snext), S is Snext + 1.

% fact: sum of all the elements in the empty list is 0
lsum([], 0).
lsum([H|T], S) :- lsum(T, Snext), S is H + Snext.


lhead([H|_], H).
% head([], Out) is false 

ltail([_|T], T).
% list([], Out) is false

% or faster: [Head|Tail] = List


% use this to construct the backtracking tree
lcontains(X, [X|_]).
lcontains(X, [H|T]) :- X \= H, lcontains(X, T). %, !. (satisfy once)


ltake(_, [], []). % empty list case
ltake(N, _, []) :- N = 0, !.
%ltake(N, L, L) :- lsize(L, N), !.
ltake(N, [H|T], [H|Rest]) :- N1 is N - 1, ltake(N1, T, Rest), !.








%% SET OPERATIONS

% set union (Ain, Bin, Rout)
% R = {x | x in A or x in B}

% base cases
set_union([], L, L). 
set_union(L, [], L).

set_union([H|T], B, R) :- member(H, B), set_union(T, B, R), !.
set_union([H|T], B, [H|R]) :- not(member(H, B)), set_union(T, B, R), !.


% set intersection (Ain, Bin, Rout)
% R = {x | x in A and x in B}

% base cases
set_intersection([], _, []).
set_intersection(_, [], []).

set_intersection([H|T], B, [H|R]) :- 
	member(H, B), set_intersection(T, B, R), !.
set_intersection([H|T], B, R) :- 
	not(member(H, B)), set_intersection(T, B, R), !.


% set difference (Ain, Bin, Rout)
% R = {x | x in A and x not in B}

% base cases
set_difference([], _, []).
set_difference(A, [], A).

set_difference([H|T], B, [H|R]) :- 
	not(member(H, B)), set_difference(T, B, R), !.
set_difference([H|T], B, R) :- 
	member(H, B), set_difference(T, B, R), !.


% generate list (start:step:lim)
gen_list(Start, _, Lim, []) :- Start > Lim, !.
gen_list(Start, Step, Lim, [Start|Rest]) :- 
	S is Start + Step, S =< Lim, gen_list(S, Step, Lim, Rest), !.
gen_list(Start, Step, Lim, [Start]) :- 
	S is Start + Step, S > Lim, !.


% complement of L in the large "World"
% complement = World - L
% here, World is the list [0, 1, 2, ..., 7] (for testing purposes)
complement(L, Out) :- gen_list(0, 1, 7, World), set_difference(World, L, Out).






%% OCW EXERCISES

firstTwo(X, Y, [H1,H2|_]) :- X = H1, Y = H2.
firstTwo2(H1, H2, [H1,H2|_]).

notContains(_, []).
notContains(E, [H|T]) :- E \= H, notContains(E, T), !.


%	unique(Lin, Lout)
unique([], []). 
unique([H|T], R) :- unique(T, R), member(H, R).
unique([H|T], [H|R]) :- unique(T, R), not(member(H, R)). 

%	listOnly(Lin, Lout)
listOnly([], []).
listOnly([H|T], R) :- listOnly(T, R), not(isList(H)).
listOnly([H|T], [H|R]) :- listOnly(T, R), isList(H). 

isList([]).
isList([_|_]).


% insertionSort(Lin, Lout)
insertionSort([], []).
insertionSort([H|T], Sorted) :- insertionSort(T, R1), insert(H, R1, Sorted).

% insert number into sorted list
insert(X, [], [X]) :- !.
insert(X, [H|T], [X,H|T]) :- X =< H, !.
insert(X, [H|T], [H|Rest]) :- X > H, insert(X, T, Rest).


% mergeSort(Lin, Lout)
mergeSort([], []) :- !.
mergeSort([X], [X]) :- !.
mergeSort(L, Sorted) :- split(L, Left, Right),
						mergeSort(Left, LL),
						mergeSort(Right, RR),
						merge(LL, RR, Sorted), !.

split(L, L1, L2) :- append(L1, L2, L),
					length(L1, N1), 
					length(L2, N2),
					(N1 = N2 ; 1 is abs(N1-N2)), !.

merge(L, [], L).
merge([], L, L).
merge([H1|T1], [H2|T2], [H1|Rest]) :- H1 < H2, merge(T1, [H2|T2], Rest), !.
merge([H1|T1], [H2|T2], [H2|Rest]) :- H2 =< H1, merge([H1|T1], T2, Rest), !.



%% TREE REPRESENTATION
% leaf(key) = node(key, nil, nil)
% node(key, left, right)

% node(1, (node(2, leaf(3), leaf(4))), (node(5, leaf(6), nil)))

% fact: the size of the empty tree is 0
tsize(nil, 0).
% fact: the size of a leaf is 1
tsize(leaf(_), 1).
tsize(node(_, L, R), S) :- tsize(L, S1), 
						   tsize(R, S2),
						   S is 1 + S1 + S2.

% fact: the height of the empty tree is 0
theight(nil, 0).
% fact: the height of a leaf is 1
theight(leaf(_), 1).
theight(node(_, L, R), H) :- theight(L, H1),
							 theight(R, H2),
							 H is 1 + max(H1, H2).

flatten(nil, []).
flatten(leaf(X), [X]).
flatten(node(K, L, R), [K|Rest]) :- flatten(L, R1), 
									flatten(R, R2), 
									append(R1, R2, Rest). 






%% EVAL
eval(val(V), R) :- R is V.
eval(add(E1, E2), R) :- eval(E1, R1), eval(E2, R2), R is R1 + R2.
eval(sub(E1, E2), R) :- eval(E1, R1), eval(E2, R2), R is R1 - R2.
eval(mult(E1, E2), R) :- eval(E1, R1), eval(E2, R2), R is R1 * R2.
