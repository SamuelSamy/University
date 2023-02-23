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

def is_prime(n):
    if n == 2:
        return True

    if n < 2 or n % 2 == 0:
        return False

    for d in range(3, int(math.sqrt(n)) + 1, 2):
        if n % d == 0:
            return False

    return True

def brute_force(n):

    if is_prime(n - 2):
        return 2

    for no in range(3, int(n / 2) + 1, 2):
        if is_prime(no) and is_prime(n - no):
            return no

    return 0

def run_tests():
    
    min_number   = 1
    max_number   = 10000
    total_tests  = 0
    failed_tests = 0

    for number in range(min_number, max_number):
        brute_force_result = brute_force(number)
        result = find_first_prime(number, generate_sieve(number))

        if result != brute_force_result:
            print(f"Error found at number = {number}\n • Value: {result}\n • Expected value: {brute_force_result}")
            failed_tests += 1

        total_tests += 1

    print (f"Testing finished.\n{total_tests - failed_tests} tests passed\n{failed_tests} tests failed")


run_tests()

