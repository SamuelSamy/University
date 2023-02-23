n = input("n = ");
p = input("p = ");

if n < 30
    fprintf("n must be greater than 30\n");
    return;
end

if p < 0.05 || p > 0.95
    fprintf("p must be between 0.05 and 0.95\n");
    return;
end

lambda = n * p;
bino = binopdf(0:n, n, p);
pois = poisspdf(0:n, lambda);

plot(0:n, bino, 0:n, pois);