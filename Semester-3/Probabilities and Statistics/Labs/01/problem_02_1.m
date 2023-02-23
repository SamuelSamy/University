x = 0:0.01:3;
f0 = x.^5 / 10;
f1 = x .* sin(x);
f2 = cos(x);

plot(x, f0, '-r', x, f1, '-g', x, f2, '-b')
title("Plots")
legend("x^5 / 10", "x * sin(x)", "cos(x)")