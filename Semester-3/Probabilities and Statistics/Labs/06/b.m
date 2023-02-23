x = [7 7 4 5 9 9 4 12 8 1 8 7 3 13 2 1 17 7 12 5 6 2 1 13 14 10 2 4 9 11 3 5 12 6 10 7];
n = length(x);
 
conf = input('conf level = ');
m0 = input('testing value = '); 

alpha = 1 - conf;
tail = 1;

[H, P, CI, STATS] = ttest(x, m0, alpha, tail);
RR = [tinv(1-alpha, n-1), inf];

if H == 1
    fprintf('The hypothesis is rejected\nThe average number of files exceds\n')
else
    fprintf('The hypothesis is not rejected\nThe average number of files does not exced\n')
end

fprintf('H = %1.0f\n', H)
fprintf('P-value = %.4f\n', P)
fprintf('CI = %.4f, %.4f\n', CI)
fprintf('TS0 = %.4f\n', STATS.tstat)
fprintf('Rejection region = (%.4f, %.4f)\n', RR)