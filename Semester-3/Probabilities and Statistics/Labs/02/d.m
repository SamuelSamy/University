format default

n = input('Please enter the number of tries: ');
p = input('Please enter the probability: ');

% P(X <= 2)
fprintf('P(X <= 2) = %.4f\n', binocdf(2, n, p));

% P(X < 2)
fprintf('P(X < 2) = %.4f\n', binocdf(2, n, p) - binopdf(2, n, p));