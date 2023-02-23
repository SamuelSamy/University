import random
from service.exceptions import CustomException


class Service:

    def __init__(self, repository):
        self.__repository = repository

    def get_rats_ate(self):
        """
        Get how much the rats ate
        :return: How much the rats ate
        """
        return self.__repository.get_rats_ate()


    def get_grain_stocks(self):
        """
        Get the grain stocks
        :return: The grain stocks
        """
        return self.__repository.get_grain_stocks()


    def get_year(self):
        """
        Get the current year
        :return: The current year
        """
        return self.__repository.get_year()

    def get_new_people(self):
        """
        Get how many new people came to the city
        :return: How many new people came to the city
        """
        return self.__repository.get_new_people()

    def get_population(self):
        """
        Get the total population
        :return: The total population
        """
        return self.__repository.get_population()

    def get_land(self):
        """
        Get how much land we have
        :return: How much land we have
        """
        return self.__repository.get_land()

    def get_harvest(self):
        """
        Get the harvest price
        :return: The harvest price
        """
        return self.__repository.get_harvest()

    def get_rats(self):
        """
        Check if we have rats or not
        :return: True if we have rats, False otherwise
        """
        return self.__repository.get_rats()

    def get_land_price(self):
        """
        Get land price
        :return: The land price
        """
        return self.__repository.get_land_price()


    def increment_year(self):
        """
        Increments the current year (year += 1)
        :return: None
        """
        self.__repository.increment_year()

    # TODO

    def automate_moves(self, acres, units_to_feed, acres_to_plant):
        """
        The main handler of all sub-functions that manage the year
        :param acres: How many acres we want to buy / sell
        :param units_to_feed: How many grains we have to feed
        :param acres_to_plant: How many acres we want to plant
        :return: None
        """
        self._verify_all(acres, units_to_feed, acres_to_plant)
        self.manage_land(acres)
        self.manage_feeding(units_to_feed)
        self.manage_harvest()
        self.manage_planting(acres_to_plant)
        self.manage_rats()


    def _verify_all(self, acres, units_to_feed, acres_to_plant):
        """
        Checks if we can perform the operations with the given inout
        :param acres: How many acres we want to buy / sell
        :param units_to_feed: How many grains we have to feed
        :param acres_to_plant: How many acres we want to plant
        :return: None
        """
        temp_stock = self.get_grain_stocks()

        if acres > 0:
            grain_price = acres * self.get_land_price()

            if grain_price > temp_stock:
                raise CustomException("You can not buy that much land!!")
            else:
                temp_stock -= grain_price


        if acres < 0:

            if abs(acres) > self.get_land():
                raise CustomException("You do not not have that much land to sell!")
            else:
                temp_stock += acres * self.get_land_price() * -1


        if units_to_feed > temp_stock:
            raise CustomException("You do not own that much grain in order to feed the people")
        else:
            temp_stock -= units_to_feed

        if acres_to_plant > self.get_land():
            raise CustomException("You can not plant more acres than you have")


        if acres_to_plant > temp_stock:
            raise CustomException("You do not have enough grains to plant that many acres")
        else:
            temp_stock -= acres_to_plant


    def manage_land(self, acres):
        """
        Buys / sells the land
        :param acres: How many acres we want to buy / sell
        :return: None
        """
        self.__repository.update_grain_stock(self.get_grain_stocks() + -1 * acres * self.get_land_price())
        self.__repository.update_land(self.get_land() + acres)
        self.__repository.set_land_price(random.randint(15, 25))


    def manage_feeding(self, units_to_feed):
        """
        Manages the feeding in the current year
        :param units_to_feed: How many grains we have to feed
        :return: None
        """
        person_grain = 20
        population = self.get_population()
        max_to_feed = units_to_feed // person_grain
        self.__repository.set_starved(max(population - max_to_feed, 0))
        self.__repository.update_grain_stock(self.get_grain_stocks() - units_to_feed)

        if self.get_starved() == 0:
            self.__repository.set_new_people(random.randint(0, 10))



    def manage_harvest(self):
        """
        Manages the harvesting in the current year
        :return:
        """
        most_acres = 10
        self.__repository.update_grain_stock(self.get_grain_stocks() + min(most_acres * self.__repository.get_population(), self.__repository.get_land()) * self.__repository.get_harvest())
        self.__repository.set_harvest(random.randint(1, 6))


    def manage_rats(self):
        """
        Manges the rats in the current year
        :return: None
        """
        grains = self.get_grain_stocks()

        if self.get_rats():
            self.__repository.update_grain_stock(grains - grains // 10)
            self.__repository.set_rats_ate(grains // 10)
        else:
            self.__repository.set_rats_ate(0)

        chance = random.randint(1, 100)

        if chance <= 20:
            self.__repository.set_rats(True)
        else:
            self.__repository.set_rats(False)



    def manage_planting(self, acres_to_plant):
        """
        Updates the stock grains
        :param acres_to_plant: How many acres will be planted
        :return: None
        """
        self.__repository.update_grain_stock(acres_to_plant * self.get_harvest())



    def game_is_over(self):
        """
        Checks if the game is over
        :return: True if we have less than 1000 acres of land or we have less than 100 people or half of the people died last year
        """
        return (self.get_land() < 1000 or self.get_population() < 100) or (self.get_population() // 2 <= self.get_starved())


    def get_starved(self):
        return self.__repository.get_starved()
