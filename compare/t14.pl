bb(1,1).
bb(2,3).
bb(3,1).
bb(4,2).
bb(2,2).
bb(a,b).

getbb2(X,Y) :- !, bb(X,Y), bb(Y,X).

getbb(X,Y) :- getbb(X,Y), !, bb(X,Y), gg(Y,X).
