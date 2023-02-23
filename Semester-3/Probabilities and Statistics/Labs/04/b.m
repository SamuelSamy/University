clear all;
clc;


n = input('Number of trials = ');
p = input('Probability of success in (0,1) = ');


U = rand(n, 1);
X = sum(U < p);


N = input('Number of simulation = ');
for i = 1:N
    U = rand(n, 1);
    X(i) = sum(U < p);
end


UX = unique(X);


nX = hist(X, length(UX));
relfr = nX / N;


k = 0:n;
pk = binopdf(k, n, p);

plot(k, pk, '*',UX, relfr, 'ro')
legend('Bino distribution','Simulation')