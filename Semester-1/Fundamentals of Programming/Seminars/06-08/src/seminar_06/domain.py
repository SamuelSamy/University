
class Car:
    
    def __init__(self, plate_number, make, model, color):
        """
        Initializes a car object

        Args:
            plate_number (string): Car's plate number
            make (string): Car's make
            model (string): Car's model
            color (string): Car's color

        Raises:
            ValueError: If either one of the parameters is an empty string
        """

        plate_number.strip()
        make.strip()
        model.strip()
        color.strip()

        if plate_number == "" or make == "" or model == "" or color == "":
            raise ValueError("Make sure you specify all the required parameters")

        self.__plate_number = plate_number
        self.__make = make
        self.__model = model
        self.__color = color

    
    @property
    def plate_number(self):
        return self.__plate_number


    @property
    def make(self):
        return self.__make


    @property
    def model(self):
        return self.__model


    @property
    def color(self):
        return self.__color


    def __str__(self):
        return f"Plate Nummber: {self.__plate_number} | Make: {self.__make} | Model: {self.__model} | Color: {self.__color}"

