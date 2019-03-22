h(2,3).
f(3).
p(11,h(4,5),f(6)).
x(W,Z) :- p(W,h(W,Z),f(W)).

% query is ?- x(W,Z). --> no
% ltest18
