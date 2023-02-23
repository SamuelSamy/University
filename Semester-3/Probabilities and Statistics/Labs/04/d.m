clear all;
clc;


p = input('Probability of success in (0,1) = ');
s = input('Number of successes = ');

N = input('Number of simulation = ');
for i = 1:N

    generated = 0;
    fails = 0;
    successes = 0;

    while (successes < s)
       last = rand;
        
       if last < p
            successes = successes + 1;
       else
            fails = fails + 1;
       end

       generated = generated + 1;
    end

    X(i) = fails;
end


UX = unique(X);


nX = hist(X, length(UX));
relfr = nX / N;


k = 0:200;
pk = nbinpdf(k, s, p);

plot(k, pk, '*',UX, relfr, 'ro')
legend('Pascal distribution','Simulation')