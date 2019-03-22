h(2,3).
f(3).
p(Z,h(W,Z),f(W)).
x(W,Z) :- p(W,h(W,Z),f(W)).

% query is ?- x(W,Z). --> Z = W
% ltest17
