x = 0:0.01:3;
f0 = x.^5 / 10;
f1 = x .* sin(x);
f2 = cos(x);

plot(x, f0, '-r', x, f1, '-g', x, f2, '-b')

subplot(3, 1, 1)
plot(x, f0, '-r')
title("Plot A")
legend("x^5 / 10")

subplot(3, 1, 2)
plot(x, f1, '-g')
title("Plot B")
legend("x * sin(x)")

subplot(3, 1, 3)
plot(x, f2, '-b')
title("Plot C")
legend("cos(x)")w