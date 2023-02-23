conflevel = input ('conf level = ');
alpha = 1 - conflevel;

Xa = [21.8, 22.6, 21.0, 19.7, 21.9, 21.6, 22.5, 23.1, 22.2, 20.1, 21.4, 20.5];
Xb = [36.5, 35.2, 36.2, 34.0, 36.4, 36.1, 37.5, 38.0, 36.3, 35.9, 35.7, 34.9];

ma = mean(Xa);
mb = mean(Xb);
m = ma - mb;

na = length(Xa);
nb = length(Xb);

va = var(Xa);
vb = var(Xb);


sp = sqrt(((na - 1) * va + (nb - 1) * vb) / (na + nb - 2));
t1 = tinv(1 - alpha / 2, na + nb - 2);
t2 = tinv(alpha / 2, na + nb - 2);
ci1 = m - t1 * sp * sqrt(1 / na + 1 / nb);
ci2 = m - t2 * sp * sqrt(1 / na + 1 / nb);
fprintf('Confidence Level (sigma1 == sigma2): (%.3f, %.3f)\n', ci1, ci2);


c = (va / na) / (va / na + vb / nb);
n = 1 / (c^2 / (na - 1) + (1 - c)^2 / (nb - 1));
t1 = tinv(1 - alpha / 2, n);
t2 = tinv(alpha / 2, n);
ci1 = m - t1 * sqrt(va / na + vb / nb);
ci2 = m - t2 * sqrt(va / na + vb / nb);
fprintf('Condifence intervals (sigma1 != sigma2): (%.3f, %.3f)\n', ci1, ci2);



f1 = finv( 1- alpha / 2, na - 1, nb - 1);
f2 = finv(alpha / 2, na - 1, nb - 1);
ci1 = 1 / f1 * va / vb;
ci2 = 1 / f2 * va / vb;
fprintf('Condifence intervals for the ratio of variances (%.3f, %.3f)\n', ci1, ci2);
fprintf('Condifence intervals for the ratio of std. deviations (%.3f, %.3f)\n', sqrt(ci1), sqrt(ci2));