
class Repositorty:

    def __init__(self):
        self.__cars = []


    def add_car(self, car):
        self.plate_already_in_list(car)
        self.__cars.append(car)


    def get_cars(self):
        cars_string = ""
        index = 0
        
        for car in self.__cars:
            cars_string += f"{index}. {car}\n"
            index += 1

        return cars_string if cars_string != "" else "• There are no cars in the program"


    def plate_already_in_list(self, car):
        for c in self.__cars:
            if c.plate_number == car.plate_number:
                raise RepoException("There's already a car with the same plate number")



class RepoException(Exception):
    
    def __init__(self, error):
        self.__error = error