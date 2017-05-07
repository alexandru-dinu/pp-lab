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

