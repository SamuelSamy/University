alpha = input('significance level (0,1) = ')

X1 = [46, 37, 39, 48, 47, 44, 35, 31, 44, 37]
X2 = [35, 33, 31, 35, 34, 30, 27, 32, 31, 31]

n1 = length(X1);
n2 = length(X2);

// a
[H, P, CI, STATS] = vartest2(X1, X2, alpha);

if H == 1 
    fprintf('The hypothesis is rejected\n') 
else
    fprintf('The hypothesis is not rejected\n')
end

f1 = finv(alpha / 2, n1 - 1, n2 - 1);
f2 = finv(1 - alpha / 2, n1 - 1, n2 - 1);

fprintf('H = %1.0f\n', H)
fprintf('P-value = %.4f\n', P)
fprintf('CI = %.4f, %.4f\n', CI)
fprintf('TS0 = %.4f\n', STATS.fstat)
fprintf('Rejection region = (%.4f, %.4f) U (%.4f, %.4f)\n', -inf, f1, f2, inf)

// b
// Find a 95% confidence interval for the difference of the average assembling times

[H, P, CI, STATS] = ttest2(X1, X2, alpha, 1, 'unequal');

if H == 1 
    fprintf('The hypothesis is rejected\n') 
else
    fprintf('The hypothesis is not rejected\n')
end

