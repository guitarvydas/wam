father(paul,albin).
father(justin,paul).
father(austina,paul).    
grandfather(X,Y) :- father(X,Z),father(Z,Y).
q :- grandfather(justin,G).
:- initialization(q).
