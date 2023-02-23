format rational

n = input('Please enter the number of tries: ');
p = input('Please enter the probability: ');

% P(X = 0)
fprintf('P(X = 0) = %.3f\n', binopdf(0, n, p))

% P(X != 1)
fprintf('P(X != 1) = %.3f\n', 1 - binopdf(1, n, p))