import random

class Service:

    def __init__(self, repository):

        self.__repository = repository


    def add_taxi(self, id, starting_location, total_fare = 0):
        return self.__repository.add_taxi(id, starting_location, total_fare)

    def get_taxis(self):
        return self.__repository.get_taxis()


    def get_sorted_taxis(self):

        taxis = self.__repository.get_taxis()

        taxis_list = []

        for taxi in taxis.values():
            taxis_list.append(taxi)

        self.sort(taxis_list)
        return taxis_list


    def sort(self, taxis):

        if len(taxis) <= 1:
            return taxis

        mid = len(taxis) // 2
        left = taxis[:mid]
        right = taxis[mid:]

        self.sort(left)
        self.sort(right)
        self.merge(left, right, taxis)


    def merge(self, left, right, result):

        i, j = 0, 0
        rez = []

        while i < len(left) and j < len(right):
            if left[i].total_fare > right[j].total_fare:
                rez.append(left[i])
                i += 1
            else:
                rez.append(right[j])
                j += 1

        while i < len(left):
            rez.append(left[i])
            i += 1

        while j < len(right):
            rez.append(right[j])
            j += 1

        result.clear()
        result.extend(rez)

    def get_closest_taxi(self, location):
        """
        Returns the closes taxi to the specified location

        :param location: City location (x, y)
        :return: The closest taxi
        """
        taxis = self.get_taxis()
        min_distance = 99999999999
        closest_taxi = None

        for taxi in taxis.values():

            distance = self.compute_distance(taxi.location, location)

            if distance < min_distance:
                min_distance = distance
                closest_taxi = taxi


        return closest_taxi


    def increment_fare(self, taxi, location1, location2):
        """
        Increments the total fare of a taxi
        :param taxi: The taxi instance
        :param location1: Start location
        :param location2: Stop location
        :return: None
        """
        self.__repository.increment_fare(taxi, self.compute_distance(location1, location2))


    def change_taxi_location(self, taxi, new_location):
        """
        Changes a location of a taxi
        :param taxi: The taxi instance
        :param new_location: New city location (x, y)
        :return: None
        """
        self.__repository.change_taxi_location(taxi, new_location)


    def handle_ride(self, start, stop):
        """
        Handles adding a ride

        :param start: Starting location
        :param stop: Stop location
        :return: The taxi that handled the ride
        """
        taxi = self.get_closest_taxi(start)
        self.increment_fare(taxi, start, stop)
        self.change_taxi_location(taxi, stop)
        return taxi


    def check_distance(self, new_location):

        taxis = self.get_taxis()

        for taxi in taxis.values():
            if self.compute_distance(taxi.location, new_location) <= 5:
                return False

        return True


    @staticmethod
    def compute_distance(location1, location2):
        return abs(location1[0] - location2[0]) + abs(location1[1] - location2[1])


    def generate_taxis(self, n):

        id = 0

        while n > 0:

            location = (random.randint(0, 100), random.randint(0, 100))
            valid_location = self.check_distance(location)

            while not valid_location:
                location = (random.randint(0, 100), random.randint(0, 100))
                valid_location = self.check_distance(location)

            try:
                self.add_taxi(id, location)
                n -= 1
                id += 1
            except:
                pass


    def generate_ride(self):

        start_location = (random.randint(0, 100), random.randint(0, 100))
        end_location = (random.randint(0, 100), random.randint(0, 100))

        while self.compute_distance(start_location, end_location) < 10:
            end_location = (random.randint(0, 100), random.randint(0, 100))

        taxi = self.handle_ride(start_location, end_location)
        return f"Ride details: Taxi ID: {taxi.id}  |  Start: {start_location}  |  Stop: {end_location}"

