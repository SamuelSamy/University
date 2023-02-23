from src.domain.taxi import Taxi
from src.exceptions import CustomException


class Repository:

    def __init__(self):
        self.__data = {}


    def add_taxi(self, id, starting_location, total_fare = 0):

        if id in self.__data.keys():
            raise CustomException("Duplicated Taxi ID")


        taxi = Taxi(id, starting_location, total_fare)
        self.__data[id] = taxi
        return taxi


    def get_taxi(self, id):
        if id not in self.__data.keys():
            raise CustomException("No Taxi with the specified ID found")

        return self.__data[id]


    def get_taxis(self):
        return self.__data


    @staticmethod
    def increment_fare(taxi, fare_to_add):
        """
        Increments the total fare of a taxi
        :param taxi: The taxi instance
        :param fare_to_add: Amount of fare to add
        :return: None
        """
        taxi.total_fare += fare_to_add

    @staticmethod
    def change_taxi_location(taxi, new_location):
        """
        Changes a location of a taxi
        :param taxi: The taxi instance
        :param new_location: New city location (x, y)
        :return: None
        """
        taxi.location = new_location
