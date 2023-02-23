from Services.functions import add_number


def add_number_ui(numbers):

    try:
        number = int(input("Enter a number: "))
    except:
        raise ValueError("The number must be an integer")

    add_number(numbers, number)
    print("• Number added")

def list_numbers(numbers):

    index = 0 

    for number in numbers:
        print(f"{index}. {number}")
        index += 1

    if index == 0:
        print("There are no numbers in the list")


def print_menu():

    print ("\n")

    print ("1. Add a number")
    print ("2. List all numbers")
    print ("3. Exit")

    print ("\n")


def main():
    
    numbers = []

    while True:     

        print_menu()
        option = input("<< Enter an opton: ")

        try:

            if option == "1":
                add_number_ui(numbers)
            elif option == "2":
                list_numbers(numbers)
            elif option == "3":
                return
            else:
                print("• Invalid option")

        except ValueError as ve:
            print(f"• {ve}")

        except:
            print("Unexpected error occured. Please try again")