from service.exceptions import CustomException

class UI:

    def __init__(self, service):
        self.service = service


    def print_data(self):
        year = self.service.get_year()
        starved = self.service.get_starved()
        new_people = self.service.get_new_people()
        population = self.service.get_population()
        land = self.service.get_land()
        harvest = self.service.get_harvest()
        rats_ate = self.service.get_rats_ate()
        land_price = self.service.get_land_price()
        grain_stocks = self.service.get_grain_stocks()

        print()
        print(f"In year {year}, {starved} people starved.")
        print(f"{new_people} people came to the city.")
        print(f"City population is {population}")
        print(f"City owns {land} acres of land.")
        print(f"Harvest was {harvest} units per acre.")
        print(f"Rats ate {rats_ate} units.")
        print(f"Land price is {land_price} units per acre.")
        print()
        print(f"Grain stocks are {grain_stocks} units.")
        print()


    def ask_for_input(self):

        valid_input = False
        acres, units_to_feed, acres_to_plant = 0, 0, 0

        while not valid_input:
            acres = input("Acres to buy/sell (+/-) -> ")
            units_to_feed = input("Units to feed the population -> ")
            acres_to_plant = input("Acres to plant-> ")

            try:
                acres = int(acres)
                units_to_feed = int(units_to_feed)
                acres_to_plant = int(acres_to_plant)
                valid_input = True
            except:
                print("Invalid input. Make sure all the numbers are integers")

        return acres, units_to_feed, acres_to_plant


    def main(self):

        year = self.service.get_year()

        self.print_data()

        while year < 5:


            try:
                acres, units_to_feed, acres_to_plant = self.ask_for_input()

                self.service.automate_moves(acres, units_to_feed, acres_to_plant)

                self.service.increment_year()
                year = self.service.get_year()

                self.print_data()

            except CustomException as error:
                print(error)


        if self.service.game_is_over():
            print(f"GAME OVER. You did not do well")
        elif year == 4:
            print("GAME OVER. You have over 100 population and 1000 acres")
