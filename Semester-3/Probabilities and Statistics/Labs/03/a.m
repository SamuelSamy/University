mode = input('(normal, student, chi2, fischer) mode = ', 's');

switch mode
    case 'normal'
        mu = input('mu = ');
        sigma = input('sigma = ');
        
        a0 = normcdf(0, mu, sigma);
        a1 = 1 - a0;
        
        b1 = normcdf(1, mu, sigma) - normcdf(-1, mu, sigma);
        b0 = 1 - b1;
        
        alpha = input('alpha = ');
        if alpha < 0 || alpha > 1
            fprintf("alpha must be between 0 and 1\n");
            return;
        end

        c = norminv(alpha, mu, sigma);

        beta = input('beta = ');
        if beta < 0 || beta > 1
            fprintf("beta must be between 0 and 1\n");
            return;
        end

        d = norminv(1 - beta, mu, sigma);

    case 'student'
        n = input('n = ');

        a0 = tcdf(0, n);
        a1 = 1 - a0;

        b1 = tcdf(1, n) - tcdf(0, n);
        b0 = 1 - b1;
       
        alpha = input('alpha = ');
        if alpha < 0 || alpha > 1
            fprintf("alpha must be between 0 and 1\n");
            return;
        end

        c = tcdf(alpha, n);

        beta = input('beta = ');
        if beta < 0 || beta > 1
            fprintf("beta must be between 0 and 1\n");
            return;
        end

        d = tcdf(1 - beta, n);


    case 'chi2'
        n = input('n = ');

        a0 = chi2cdf(0, n);
        a1 = 1 - a0;

        b1 = chi2cdf(1, n) - chi2cdf(0, n);
        b0 = 1 - b1;

        alpha = input('alpha = ');
        if alpha < 0 || alpha > 1
            fprintf("alpha must be between 0 and 1\n");
            return;
        end

        c = chi2cdf(alpha, n);

        beta = input('beta = ');
        if beta < 0 || beta > 1
            fprintf("beta must be between 0 and 1\n");
            return;
        end

        d = chi2cdf(1 - beta, n);

    case 'fischer'
        m = input('m = ');
        n = input('n = ');

        a0 = fcdf(0, m, n);
        a1 = 1 - a0;

        b0 = fcdf(1, m, n) - fcdf(0, m, n);
        b1 = 1 - b1;

        alpha = input('alpha = ');
        if alpha < 0 || alpha > 1
            fprintf("alpha must be between 0 and 1\n");
            return;
        end

        c = fcdf(alpha, m, n);

        beta = input('beta = ');
        if beta < 0 || beta > 1
            fprintf("beta must be between 0 and 1\n");
            return;
        end

        d = fcdf(1 - beta, m, n);


    otherwise
        disp("Invalid mode");
        return;
end

fprintf('P(x <= 0) = %.4f\nP(x >= 0) = %.4f\n', a0, a1);
fprintf('P(-1 <= x <= 1) = %.4f\nP(x <= 1 or x >= 1) = %.4f\n', b0, b1);
fprintf('P(X < Xa) = a, X = %.4f\n', c);
fprintf('P(X > Xb) = b, X = %.4f\n', d);