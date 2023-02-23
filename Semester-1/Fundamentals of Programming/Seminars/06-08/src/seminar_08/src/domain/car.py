"""
Implement the domain class
"""
import random
import string

CAR_COUNTIES = ['CJ', 'B', 'BN', 'SJ', 'AR']
CAR_MAKES_AND_MODELS = {
    "Dacia": ['Lodgy', 'Dokker'],
    "BMW": ['Serie 1', 'X5']
}
CAR_COLORS = ['blue', 'red', 'green', 'black']

class Car:

    def __init__(self, id_, license_plate="n/a", make='', model='', color=''):
        
        self._id = id_
        self._license_plate = license_plate
        self._make = make
        self._model = model
        self._color = color


    @property
    def id(self):
        return self._id


    @property
    def license_plate(self):
        return self._license_plate


    @license_plate.setter
    def license_plate(self, new_license_plate):
        self._license_plate = new_license_plate


    @property
    def make(self):
        return self._make

    @property
    def model(self):
        return self._model

    @property
    def color(self):
        return self._color

    @color.setter
    def color(self, new_color):
        self._color = new_color

    # car_1 == car_2
    def __eq__(self, z):
        if isinstance(z, Car) is False:
            return False
        return self.id == z.id

    def __str__(self):
        return f"ID: {self.id}  |  License Plate: {self.license_plate} | Make: {self.make} | Model: {self.model} | Color: {self.color}"

    def __repr__(self):
        return str(self)


def generate_cars(n=50):
    """
    Generate a number of cars
    :param n:
    :return: List of n cars generated pseudo-randomly
    """
    result = []

    for i in range(n):

        id = random.randrange(5 * i, 5 * i + 4)

        county = random.choice(CAR_COUNTIES)
        
        number = random.randrange(1, 100)
        number = str(number) if number > 9 else '0' + str(number)

        letters = random.choices(string.ascii_uppercase, k = 3)
        letters = letters[0] + letters[1] + letters[2]

        license_plate = f"{county} {number} {letters}"
        make = random.choice(list(CAR_MAKES_AND_MODELS.keys()))
        model = random.choice(CAR_MAKES_AND_MODELS[make])
        color = random.choice(CAR_COLORS)

        car = Car(id, license_plate, make, model, color)

    result.append(car)

    return result
