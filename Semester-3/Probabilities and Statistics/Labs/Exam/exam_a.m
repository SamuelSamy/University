Xa = [21.8, 22.6, 21.0, 19.7, 21.9, 21.6, 22.5, 23.1, 22.2, 20.1, 21.4, 20.5];
Xb = [36.5, 35.2, 36.2, 34.0, 36.4, 36.1, 37.5, 38.0, 36.3, 35.9, 35.7, 34.9];

na = length(Xa);
nb = length(Xb);

alpha = input("alpha level = ");

% tail: two sided
[H, P, CI, STATS] = vartest2(Xa, Xb, alpha);

if H == 1
    fprintf('The hypothesis is rejected. The population variances do not seem to differ\n') 
else
    fprintf('The hypothesis is not rejected. The population variances do seem to differ\n')
end

f1 = finv(alpha / 2, na - 1, nb - 1);
f2 = finv(1 - alpha / 2, na - 1, nb - 1);

fprintf('H = %1.0f\n', H)
fprintf('P-value = %.4f\n', P)
fprintf('CI = %.4f, %.4f\n', CI)
fprintf('TS0 = %.4f\n', STATS.fstat)
fprintf('Rejection region = (%.4f, %.4f) U (%.4f, %.4f)\n', -inf, f1, f2, inf)

