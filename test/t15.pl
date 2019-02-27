bb(1,1).
bb(2,3).
bb(3,1).
bb(4,2).
bb(2,2).
bb(a,b).

getbb(X,_) :- bb(X,1), !.
getbb(X,Y) :- bb(X,Y).
