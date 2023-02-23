from src.domain.exceptions import ConnectException
from src.domain.piece import Piece


class Cell:

    def __init__(self, row, col):
        self._piece = None
        self._row = row
        self._col = col

        
    @property
    def piece(self):
        """
        :return: The piece object
        """
        return self._piece

    @piece.setter
    def piece(self, new_piece):
        """
        Sets the piece to a new value
        :param new_piece: The new value
        """
        if new_piece is not None and not isinstance(new_piece, Piece):
            raise ConnectException(f"Invalid piece type\nExpected {type(Piece)} got {type(new_piece)}")

        self._piece = new_piece


    @property
    def row(self):
        """
        :return: The row of the cell
        """
        return self._row


    @property
    def col(self):
        """
        :return: The column of the cell
        """
        return self._col


    def is_empty(self):
        """
        :return: True, if the cell does not contain a piece, False otherwise
        """
        return self._piece is None

    def __str__(self):
        """
        Converts to string the `Cell` object
        :return: A string representing the current `Cell` object
        """
        return f"Piece: {self.piece}  |  Row: {self.row}  |  Col: {self.col}"
