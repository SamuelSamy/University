class ConnectException(Exception):
    def __init__(self, message):
        self.message = message


class IndexOutOfMatrixBounds(Exception):

    def __init__(self, message = ""):
        self.message = message
