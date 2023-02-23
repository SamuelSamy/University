n = input('Please enter the number of tries: ');
p = input('Please enter the probability: ');

heads = 0;
values = rand(n);

for i = 1:length(values)
    value = values(i);

    if value < 0.5 
        fprintf("Heads ");
        heads = heads + 1;
    else
        fprintf("Tails ");
    end
end

fprintf("\nX = %d\n%.4f\n", heads, binopdf(heads, n, p));
