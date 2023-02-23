X1 = [22.4 21.7 24.5 23.4 21.6 23.3 22.4 21.6 24.8 20.0];
X2 = [17.7 14.8 19.6 19.6 12.1 14.8 15.4 12.6 14.0 12.2];

n1 = length(X1);
n2 = length(X2);

alpha = input('alpha = ');

[H, P, CI, STATS] = vartest2(X1, X2, alpha);

if H == 1 
    fprintf('The hypothesis is rejected.\n') 
else
    fprintf('The hypothesis is not rejected.\n')
end


f1 = finv(alpha / 2, n1 - 1, n2 - 1);
f2 = finv(1 - alpha / 2, n1 - 1, n2 - 1);

fprintf('H = %1.0f\n', H)
fprintf('P-value = %.4f\n', P)
fprintf('CI = %.4f, %.4f\n', CI)
fprintf('TS0 = %.4f\n', STATS.fstat)
fprintf('Rejection region = (%.4f, %.4f) U (%.4f, %.4f)\n', -inf, f1, f2, inf)



