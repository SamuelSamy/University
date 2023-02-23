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


    days = date['day']
    days += sum(month_days[0 : date['month'] - 1])
    days += year * 365


    leap_years = int(year / 4) - int(year / 100) + int(year / 400) 
    days += leap_years

    return days

#include <vector>
int get_days(int year, int month, int day)
{
    int totalDays = 0;

    std::vector<int> v{[31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};

    if is_leap_year(year)
    {
        v[1] = 29;
    }

    totalDays = day;
    
    for (int i = 0; i < month; i++)
    {
        totalDays += v[i];
    }

    totalDays += (year - 1) * 365;
    totalDays += year / 4 - year / 100 + year / 400

    return totalDays;
}

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


def solve():

    birth_date = read_date("date of birth")
    current_date = read_date("current date")

    if not is_valid_date(birth_date) or not is_valid_date(current_date):
        print("Please enter a valid input")
    else:

        birth_days = get_days(birth_date)
        current_days = get_days(current_date)

        if current_days - birth_days < 0:
            print("Please enter a valid input (Current Date > Birth Date)")
        else:
            print(f"This person is {current_days - birth_days} days old.")


solve()