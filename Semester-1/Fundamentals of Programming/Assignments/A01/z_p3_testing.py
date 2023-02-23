import math
import random

from datetime import date, datetime, timedelta

def read_date(details):

    date = {
        "year": 0,
        "month": 0,
        "day": 0
    }

    print(f"Enter {details}:")

    date['year'] = int(input(" • year: "))
    date['month'] = int(input(" • month: "))
    date['day'] = int(input(" • day: "))

    return date

def get_days(date):

    month_days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

    if is_leap_year(date['year']):
        month_days[1] = 29

    year = date['year'] - 1

    leap_years = int(year / 4) - int(year / 100) + int(year / 400) 

    days = date['day']
    days += sum(month_days[0 : date['month'] - 1])
    days += year * 365
    days += leap_years

    return days


def is_leap_year(year):
    return (year % 4 == 0 and year % 100 != 0 or year % 400 == 0)


def is_valid_date(date):

    month_days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

    if date['day'] < 1 or date['month'] < 1 or date['year'] < 1 or date['month'] > 12:
        return False

    if is_leap_year(date['year']) and date['month'] == 2:
        month_days[1] = 29

    if date['day'] > month_days[date['month'] - 1]:
        return False

    return True


def convert_to_custom_format(date):

    custom_date_oject = {
        "year": date.year,
        "month": date.month,
        "day": date.day
    }

    return custom_date_oject

def run_tests():

    tests = 1000000
    failed_tests = 0

    for i in range(tests):

        birth_date = random_date(date(1900, 1, 1))
        current_date = random_date(birth_date)

        custom_birth_date = convert_to_custom_format(birth_date)
        custom_current_date = convert_to_custom_format(current_date)

        if not is_valid_date(custom_birth_date) or not is_valid_date(custom_current_date):
            print("Please enter a valid input")
            return

        birth_days = get_days(custom_birth_date)
        current_days = get_days(custom_current_date)

        days = current_days - birth_days

        if days < 0:
            print("Please enter a valid input (Current Date > Birth Date)")
            return

        birth_date_funcs = date(custom_birth_date['year'], custom_birth_date['month'], custom_birth_date['day'])
        current_date_funcs = date(custom_current_date['year'], custom_current_date['month'], custom_current_date['day'])

        actual_days = (current_date_funcs - birth_date_funcs).days

        if days != actual_days:
            print (f"\nError found at:\n • Birth date = {custom_birth_date}\n • Current date = {custom_current_date}\n • Value: {days}\n • Expected value: {actual_days}\n")
            failed_tests += 1

    print (f"Testing finished\n{tests - failed_tests} tests passed\n{failed_tests} tests failed")


def random_date(start_date):

    max_date = date(2199, 12, 31)

    days = (max_date - start_date).days

    if days < 2:
        days = 2

    return start_date + timedelta(days = random.randrange(1, days))


def run_test_single_date(_date):
    
    custom_date_object = convert_to_custom_format(_date)

    days = get_days(custom_date_object)
    actual_days = (_date - date(1, 1, 1)).days + 1

    if days != actual_days:
        print (f"\nError found at date = {custom_date_object}\n • Value: {days}\n • Expected value: {actual_days}\n")
    else:
        print (f"No errors found for date = {custom_date_object}")



# run_test_single_date(date(2056, 5, 22))
# run_test_single_date(date(2179, 7, 3))

run_tests()