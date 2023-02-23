import math

def generate_sieve(n):

    marked = [False] * (n + 1)

    marked[0] = marked[1] = True

    for i in range(4, n, 2):
        marked[i] = True

    for i in range(3, int(math.sqrt(n)) + 1, 2):
        if not marked[i]:
            for j in range(i * i, n, 2 * i):
                marked[j] = True

    return marked


def find_first_prime(n, marked):

    if not marked[n - 2]:    
        return 2
    else:
        for number in range(3, int(n / 2) + 1, 2):
            if not marked[number] and not marked[n - number]:
                return number

    return 0


def solve():
    n = int(input("Enter a number\nn = "))

    marked = generate_sieve(n)

    p1 = find_first_prime(n, marked)
    p2 = n - p1

    if p1 != 0:
        print(f"p1 = {p1}\np2 = {p2}")
    else:
        print(f"There are no prime numbers that add up to {n}")


solve()
