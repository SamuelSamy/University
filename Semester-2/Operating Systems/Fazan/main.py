
class Player:

    def __init__(self, ID) -> None:
        self.__ID = ID

    @property
    def ID(self):
        return self.__ID
