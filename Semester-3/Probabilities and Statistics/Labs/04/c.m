clear all;
clc;


p = input('Probability of success in (0,1) = ');


N = input('Number of simulation = ');
for i = 1:N

    generated = 1;
    last = rand;
    while (last >= p)
       last = rand;
       generated = generated + 1;
    end

    X(i) = generated;
end


UX = unique(X);


nX = hist(X, length(UX));
relfr = nX / N;


k = 0:20;
pk = geopdf(k, p);

plot(k, pk, '*',UX, relfr, 'ro')
legend('Geo distribution','Simulation')