clear all;
clc;

n = input('Number of simulations = ');
p = input('Probability of success = ');

X = (1:n);

%for i = 1:n
%   U = rand;
%   X(i) = (U < p);
%end

U = rand(1, n);
X = (U < p);

U_X = unique(X);
n_X = hist(X, length(U_X));
relative_freq = n_X / n;
[U_X; relative_freq]

