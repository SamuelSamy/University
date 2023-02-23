format rational

n = input('Please enter the number of tries: ');
p = input('Please enter the probability: ');
k = 0:n;

values_p = binopdf(k, n, p);
xx = 0:0.01:n;
values_c = binocdf(xx, n, p);


A = [k; values_p];
disp(A)
plot(k, values_p, '*')
hold on
plot(xx, values_c, '*')
legend('pdf', 'cdf')
hold off
