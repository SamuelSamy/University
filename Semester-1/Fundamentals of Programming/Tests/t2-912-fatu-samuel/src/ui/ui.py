

class UI:

    def __init__(self, service):

        self.__service = service



    def start(self):

        valid_input = False
        n = 0

        while not valid_input:

            try:
                n = int(input("Enter number of taxis: "))
                valid_input = True
            except:
                valid_input = False

        self.__service.generate_taxis(n)
        self._print_taxis()

        while True:
            self._print_menu()
            choice = input("Enter an option: ").strip()

            if choice == "1":
                self.add_ride()
                self._print_taxis()
            elif choice == "2":
                self.generate_rides()
            elif choice == "3":
                break
            else:
                print("Invalid option")


    def _print_taxis(self):
        taxis = self.__service.get_sorted_taxis()
        for taxi in taxis:
            print(taxi)


    @staticmethod
    def _print_menu():
        print("1. Add a ride")
        print("2. Simulate rides")
        print("3. Exit")


    def add_ride(self):

        start_x = int(input("Enter x for starting location: "))
        start_y = int(input("Enter y for starting location: "))
        stop_x = int(input("Enter x for finish location: "))
        stop_y = int(input("Enter y for finish location: "))

        self.__service.handle_ride((start_x, start_y), (stop_x, stop_y))


    def generate_rides(self):

        n = int(input("Enter number of rides: "))

        while n > 0:

            ride = self.__service.generate_ride()
            print(ride)
            self._print_taxis()

            n -= 1

