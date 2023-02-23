class UIException(Exception):

    def __init__(self, message):
        self.__message = message

    @property
    def message(self):
        return self.__message

    def __str__(self):
        return self.message