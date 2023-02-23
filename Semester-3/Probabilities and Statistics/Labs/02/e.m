format default

n = input('Please enter the number of tries: ');
p = input('Please enter the probability: ');

% P(X >= 1)
fprintf('P(X >= 1) = %.4f\n', binocdf(n, n, p) - binopdf(.99, n, p));

% P(X > 1)
fprintf('P(X > 1) = %.4f\n', binocdf(n, n, p) - binocdf(1, n, p));