def generate_digits_list(n):

    digits = [False] * 10

    while n:
        digits[n % 10] = True
        n //= 10

    return digits


def check_for_property_p(digits_list1, digits_list2):

    for i in range(0, 10):
        if digits_list1[i] != digits_list2[i]:
            return False

    return True

def solve():
    n1 = int(input("Enter the first number: "))
    n2 = int(input("Enter the second number: "))

    digits_list_n1 = generate_digits_list(n1)
    digits_list_n2 = generate_digits_list(n2)

    has_property_p = check_for_property_p(digits_list_n1, digits_list_n2)

    if has_property_p:
        print("The two given numbers have property `p`")
    else:
        print("The two given numbers do not have property `p`")


solve()
    