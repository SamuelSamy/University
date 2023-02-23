from services.services import Service
from z_packages.termcolor import colored

class UI:

    def __init__(self) :
        self.__services = Service()

    
    def __print_menu(self):

        print("\n")

        print(colored(">> 1. Add student", "blue"))
        print(colored(">> 2. List all students", "blue"))
        print(colored(">> 3. Filter students", "blue"))
        print(colored(">> 4. Undo the last operation", "blue"))
        print(colored(">> 5. Exit", "blue"))

        print("\n")

    
    def main(self):
        
        self.__services.generate_students()
        index = 0

        while True:
                
            self.__print_menu()

            option = input("<< Enter an option: ").strip()

            try:
                if option == "1":
                    self.__handle_add_student(index)
                    index += 1
                elif option == "2":
                    self.__list_students()
                elif option == "3":
                    self.__handle_filter_students(index)
                    index += 1
                elif option == "4":
                    self.__undo_last_operation(index)
                    index -= 1
                elif option == "5":
                    return
                else:
                    print(colored("• Invalid Option", "red"))
            except ValueError as ve:
                print(colored(f"• {ve}", "red"))


    def __handle_add_student(self, index):
        
        id = input("<< Enter student's ID: ")
        name = input("<< Enter student's name: ")
        group = input("<< Enter student's group: ")

        self.__services.add_student(self.__services.create_student(id, name, group), index)

        print (colored("• Student added succesfully!", "green"))


    def __list_students(self):

        students = self.__services.students

        if len(students) != 0:
            for student in students:
                print(colored(str(student), "green"))
        else:
            print(colored("• There are no students to display!", "red"))


    def __handle_filter_students(self, index):
        
        group = input("<< Enter a group: ")

        self.__services.filter_students(group, index)

        print(colored("• Studentds filtered!", "green"))


    def __undo_last_operation(self, index):
        
        if len(self.__services.operations) == 0:
            raise ValueError("No operations left to undo!")

        self.__services.undo_last_operation(index)

        print(colored("• Last operation reverted", "green"))