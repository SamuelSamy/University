#  https://codeshare.io/PdPDJY

def read_numbers():
    n = float(input("Enter n: "))
    r = int(input("Enter root: "))
    p = float(input("Enter precision: "))

    return n, r, p


def exp_power(number, power):

    if power == 0:
        return 1

    if power == 1:
        return number

    if power % 2 == 0:
        return exp_power(number * number, int(power / 2)) 

    return number * exp_power(number * number, int(power / 2))


def search_root(lower_bound, upper_bound, left, right, power):
    
    mid = (left + right) / 2

    number = exp_power(mid, power)

    if lower_bound <= number <= upper_bound:
        return mid

    if number < lower_bound:
        return search_root(lower_bound, upper_bound, mid + 1, right, power)
    
    return search_root(lower_bound, upper_bound, left, mid - 1, power)


if __name__ == "__main__":
    while True:
        n, r, p = read_numbers()

        if p != 0:
            p = 10 ** (-p)

        root = search_root(n - p, n + p, 1, n, r)

        print(f"The root is {root}\n")
