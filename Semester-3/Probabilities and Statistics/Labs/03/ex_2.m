p = input("p = ");

if p < 0.05 || p > 0.95
    fprintf("p must be between 0.05 and 0.95\n");
    return;
end

for i = 1:2:1000
    bino = binopdf(0:i, i, p);
    mu = i * p;
    sigma = sqrt(mu * (1 - p));
    norm = normpdf(0:i, mu, sigma);

    plot(0:i, bino, 0:i, norm);
    pause(.5);
end
