import random
import time

def get_codes(secret_number, number):

    codes = 0
    number = get_digits(number)

    for i in range(0, 4):
        if secret_number[i] == number[i]:
            codes += 1

    return codes


def get_runners(secret_number, number):
    
    runners = 0
    number = get_digits(number)

    for digit in number:
        if digit in secret_number:
            runners += 1

    return runners


def build_number(digits):
    number = ""

    for digit in digits:
        number += str(digit)

    return number


def get_digits(number):

    digits = []

    for digit in number:
        digits.append(int(digit))

    return digits


def is_valid_guess(number):

    digits = get_digits(number)

    if digits[0] == 0: 
        return False

    for i in range(0, 4):
        for j in range(0, 4):
            if i != j and digits[i] == digits[j]:
                return False

    return True


def generate_number():

    """
    Generates a 4 digits number

    The function initializes a list that contains all possible digits and it shuffles it
    Afterwards, if the first element of the list (NUMBER_LIST[0]) is not 0:
                    we return a list that contains the elemtnts from position 0 to position 3
                else
                    we return a list that conatins the elements from position 1 to 4

    Returns:
        [list]: A list containing 4 random distinct digits
    """

    NUMBER_LIST = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
    random.shuffle(NUMBER_LIST) 
    index = 0 if NUMBER_LIST[0] != 0 else 1
    return NUMBER_LIST[index : 4 + index]   # 0:4 if the first digit is not 0
                                            # 1:5 if the first digit is 0


def game_ended(start_timestamp, max_game_time):

    if round(start_timestamp) + max_game_time <= round(time.time()):
        return True
    
    return False


def test_generate_number():

    tests = 100

    while tests > 0:

        number = generate_number()
        number = build_number(number)

        assert is_valid_guess(number) == True

        tests -= 1

test_generate_number()