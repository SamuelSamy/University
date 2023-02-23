from repository import Repositorty, RepoException
from services import Services

class UI:

    def __init__(self):
        self.__services = Services()


    def main(self):

        while True:

            self.__print_menu()

            user_option = input("<< Enter an option: ")

            try:
                if user_option == "1":
                    self.handle_car_adding()
                elif user_option == "2":
                    self.list_all_cars()
                elif user_option == "3":
                    return
                else:
                    print ("• Invalid option")
            except RepoException as re:
                print(f"• {re}")
            except ValueError as ve:
                print(f"• {ve}")
            except:
                print("• Unexpected error occured")

    
    
    def __print_menu(self):

        print ("\n")
        print (">> 1. Add a car")
        print (">> 2. List all cars")
        print (">> 3. Exit")
        print ("\n")


    def handle_car_adding(self):

        plate_number = input("<< Enter the plate's number: ")
        make = input("<< Enter car's make: ")
        model = input("<< Enter car's model: ")
        color = input("<< Enter car's color: ")

        self.__services.add_car(plate_number, make, model, color)

        print ("• Car added successfully")


    def list_all_cars(self):
        to_print = self.__services.get_cars()
        print(to_print)