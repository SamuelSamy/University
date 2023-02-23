from repository import Repositorty
from domain import Car


class Services:
    
    def __init__(self):
        self.__cars = Repositorty()

    
    def add_car(self, plate_number, make, model, color):

        car = Car(plate_number, make, model, color)

        self.__cars.add_car(car)


    def get_cars(self):
        return self.__cars.get_cars()