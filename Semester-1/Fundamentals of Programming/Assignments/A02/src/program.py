# UI section

def main():
    complex_numbers = init_numbers()

    while True:
        
        print_menu()

        option = input("Select option: ")

        if option == "1":
            add_complex_number_ui(complex_numbers)
        elif option == "2":        
            display_numbers(complex_numbers)
        elif option == "3":
            generate_longest_sequence_ui(select_property(), complex_numbers)
        elif option == "4":
            return
        else:
            print("Invalid option")


def print_menu():
    print("1. Add complex number")
    print("2. Display all complex numbers")
    print("3. Display the longest sequence that observes a given property")
    print("4. Exit")


def select_property():

    print("1. Numbers having increasing modulus")
    print("2. Both real and imaginary parts can be written using the same base 10 digits")

    _property = input("Select a property: ")

    return _property


def generate_longest_sequence_ui(_property, complex_numbers): 
    
    longest_sequence = []

    if _property == "1": # Numbers having increasing modulus
        longest_sequence = get_longest_sequence_having_increasing_modulus(complex_numbers)
    elif _property == "2": # Both real and imaginary parts can be written using the same base 10 digits
        longest_sequence = get_longest_sequence_having_the_same_base_digits(complex_numbers)
    else:
        print("Invalid option")
        return
    
    if longest_sequence != []:
        display_sequence(longest_sequence)
    else:
        print("Theere is no sequence that obeserves the given property")


def add_complex_number_ui(numbers_list):
    
    try:
        real = int(input("Enter the real value: "))
        imaginary = int(input("Enter the imaginary value: "))
    except:
        print ("Please make sure the numbers are valid")
        return

    complex_number = create_complex_number(real, imaginary)
    add_complex_number(numbers_list, complex_number)
    
    print("Number added successfully")


def display_numbers(numbers_list):
    index = 1
    for number in numbers_list:
        print(f"{index}. {build_display_string(number)}")
        index += 1


def display_sequence(numbers_list):
    message = "The longest sequence that observes the given property is: "

    for number in numbers_list:
        message += f"{build_display_string(number)}, "
    
    message = message[:-2]

    print(message)


def build_display_string(number):
    """
    Build a string that represents the complex number

    Args:
        number ({"real", "imaginary"}): A complex number

    Returns:
        string: A string that represents the complex number (a + bi)
    """
    
    real = get_real_value(number)
    imaginary = get_imaginary_value(number)
    sign = get_sign(number)

    response = ""

    if real != 0:
        response += f"{real} "

        if sign != '': # imaginary != 0
       
            if imaginary == 1:
                response += f"{sign} i"
            else:
                response += f"{sign} {abs(imaginary)}i"
    else:
        if sign != '':
            response += f"{imaginary}i"

    if real == 0 and imaginary == 0:
        response = "0"

    return response


def get_sign(number):
    """
    Returns the sign of the complex numer

    Args:
        number ({"real", "imaginary"}): A complex number

    Returns:
        string: '+' if the imaginary value is greater than 0, 
                '-' if the imaginary value is less than 0,
                ''  if the imaginary value is 0 
    """

    imaginary = get_imaginary_value(number)

    if imaginary > 0:
        return '+'
    elif imaginary < 0:
        return '-'

    return ''

# Function section

def init_numbers():
    
    complex_numbers = []

    # numbers for incrasing modulus

    complex_numbers.append(create_complex_number(1, 1))
    complex_numbers.append(create_complex_number(2, 1))
    complex_numbers.append(create_complex_number(3, 3))
    complex_numbers.append(create_complex_number(1, 0))
    complex_numbers.append(create_complex_number(4, 5))

    # numbers for same base digits

    complex_numbers.append(create_complex_number(1, 1))
    complex_numbers.append(create_complex_number(11, 1))
    complex_numbers.append(create_complex_number(11, 11))
    complex_numbers.append(create_complex_number(0, 1))
    complex_numbers.append(create_complex_number(3, 333))

    return complex_numbers


def create_complex_number(real, imaginary):
    complex_number = {
        "real": real,
        "imaginary": imaginary
    }

    return complex_number


def add_complex_number(numbers_list, number):
    numbers_list.append(number)


def get_real_value(number):
    return number["real"]


def get_imaginary_value(number):
    return number["imaginary"]


def get_longest_sequence_having_increasing_modulus(complex_numbers):
    """
    Finds the longest sequence having increasing modulus.

    Args:
        complex_numbers: [{"real", "imaginary"]: A lits containing complex numbers

    Returns:
        list: The longest sequence of complex numbers having increasing modulus
    """

    last_modulus = float('inf') 

    current_sequence = []
    longest_sequence = []

    for complex_number in complex_numbers:
        
        current_modulus = get_modulus(complex_number)

        if current_modulus <= last_modulus:
            if len(current_sequence) > len(longest_sequence):
                longest_sequence = current_sequence.copy()

            current_sequence.clear()
    

        current_sequence.append(complex_number)
        last_modulus = current_modulus
        

    if len(current_sequence) > len(longest_sequence):
        longest_sequence = current_sequence

    return longest_sequence


def get_modulus(complex_number):
    """
    Gets the square of a complex number

    Args:
        complex_number: {"real", "imaginary"}: A complex number

    Returns:
        int: For a complex nubmer z = a + bi -> |z|^2 = (sqrt(a * a + b * b))^2 = a * a + b * b
    """

    real = get_real_value(complex_number)
    imaginary = get_imaginary_value(complex_number)

    return real ** 2 + imaginary ** 2


def get_longest_sequence_having_the_same_base_digits(complex_numbers):
    """
    Finds the longest sequence where both real and imaginary parts can be written using using the same base digits in base 10.

    Args:
        complex_numbers: [{"real", "imaginary"}]: A list of complex numbers

    Returns:
        list:   - An empty list if there are no complex numbers
                - The longest sequence having the same base digits
    """

    current_sequence = []
    longest_sequence = []

    last_number_digits = [False] * 10

    for complex_number in complex_numbers:

        current_numbers_digits = generate_digits_list(complex_number)

        if last_number_digits != current_numbers_digits:

            if len(current_sequence) > len(longest_sequence):
                longest_sequence = current_sequence.copy()

            current_sequence.clear()

        current_sequence.append(complex_number)
        last_number_digits = current_numbers_digits.copy()
        
    if len(current_sequence) > len(longest_sequence):
        longest_sequence = current_sequence

    return longest_sequence

1 2 3 2 1

def generate_digits_list(complex_number):
    """
    Creates and returns a list such that digits[i] = True if and only if the complex number contains the digit `i`

    Args:
        complex_number {"real", "imaginary"}: A complex number

    Returns:
        list: A list of 10 bool values ; - if the number has the digit `i` digits[i] is True, False otherwise
    """

    digits = [False] * 10

    mark_digits(get_real_value(complex_number), digits)
    mark_digits(get_imaginary_value(complex_number), digits)

    return digits


def mark_digits(number, digits):
    """
    Modifies the `digits` list such that digits[i] = True if and only if `number` contains the digit `i`
    """

    if number == 0:
        digits[0] = True

    while number != 0:
        digits[number % 10] = True

        number //= 10


main()
