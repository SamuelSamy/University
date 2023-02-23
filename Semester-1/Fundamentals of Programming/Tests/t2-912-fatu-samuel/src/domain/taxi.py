

class Taxi:

    def __init__(self, ID, location, total_fare = 0):

        self.__id = ID
        self.__location = location
        self.__total_fare = total_fare


    @property
    def id(self):
        return self.__id

    @property
    def location(self):
        return self.__location

    @property
    def total_fare(self):
        return self.__total_fare

    @total_fare.setter
    def total_fare(self, new_value):
        self.__total_fare += new_value

    @location.setter
    def location(self, new_value):
        self.__location = new_value


    def __str__(self):
        return f"{str(self.id).ljust(5)} {str(self.location).rjust(10)} {str(self.total_fare).rjust(10)}"
