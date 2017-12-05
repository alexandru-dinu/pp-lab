% define Knowledge Base
a(1).

b(1).
b(2).

c(1).
c(2).

d(2).

e(2).

f(3).



% version 1 - without cut
p1(X):-  a(X).
p1(X):-  b(X),  c(X),  d(X),  e(X).
p1(X):-  f(X).



% version 1 - with cut
p2(X):-  a(X).
p2(X):-  b(X),  c(X), !,  d(X),  e(X).
p2(X):-  f(X).


g(X) :- a(X).
mf(_) :- fail.

mnot(Goal) :- Goal, !, fail.
mnot(_).


%% 1.
q(a, b).
%% X = a, Y = b done

%% 2
x(a, b).
x(b, a).

y(X, Y) :- x(Y, X), !.
%% X = b, Y = a, done

%% 3
r(a, b).
r(b, a).

r(X, Y) :- r(Y, X), !.
%% X = a, Y = b from KB x(a, b)
%% X = b, Y = a from KB x(b, a)
%% X = b, Y = a from the above predicate and cut

%% 4
z(a, b).
z(b, a).

z(X, Y) :- !.
%% X = a, Y = b from KB z(a, b)
%% X = b, Y = a from KB z(b, a)
%% true from the last predicate and cut

%% 5
l(a, b).
l(b, a).

l(X, Y) :- !.
l(X, Y) :- l(Y, X).
%% X = a, Y = b from KB l(a, b)
%% X = b, Y = a from KB l(b, a)
%% true from cut (already satisfied)

%% 6
m(a, b).
m(b, a).

m(X, Y) :- m(X, X), !.
%% out of stack
