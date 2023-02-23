import copy
from src.domain.cell import Cell
from src.domain.exceptions import IndexOutOfMatrixBounds
from src.domain.piece import Piece


class GameRepository:

    def __init__(self, rows = 6, cols = 7):
        self._rows = rows
        self._cols = cols

        self._board = [[Cell(j, i) for i in range (cols)] for j in range (rows)]

    def get_copy_board(self):
        """
        :return: A copy of the current board
        """
        return copy.deepcopy(self._board)


    def place_piece(self, col, piece_owner, piece_color = None, piece_text = None):
        """
        Place a piece in the specified column
        :param col: The column in which the piece is going to be placed
        :param piece_owner: The owner of the piece
        :param piece_color: A (string) color representing the piece color (Used for console)
        :param piece_text:A string representing the piece text (Used for console)
        :return: None
        """
        row = self.get_next_row(col)
        self._board[row][col].piece = Piece(piece_owner, piece_color, piece_text)


    def is_valid_column(self, col):
        """
        Checks if any piece can be placed on the specified column
        :param col: The column we want to check
        :return: True, if we can place the piece, False otherwise
        """
        return self._board[0][col].is_empty()

    
    def get_next_row(self, col):
        """
        Gets the next available row in the specified column
        :param col: The column we want to check
        :return: -1 if the column is full, otherwise the row index
        """
        for i in reversed(range(self._rows)):
            if self._board[i][col].is_empty():
                return i

        return -1


    def get_cell(self, row, col):
        """
        Gets a `Cell` object by its index in the board
        :param row: The row in which the cell is in
        :param col: The column in which the cell is in
        :return: A `Cell` Object
        """
        if not self.is_valid_position(row, col):
            raise IndexOutOfMatrixBounds()

        return self._board[row][col]


    def is_valid_position(self, row, col):
        """
        Checks if the specified position is a valid one
        :param row: The row we want to check
        :param col: The column we want to check
        :return: True, the specified coordinates are valid, False otherwise
        """
        return self._rows > row >= 0 and self._cols > col >= 0

    
    def get_total_rows(self):
        """
        :return: The total amount of rows
        """
        return self._rows

    
    def get_total_cols(self):
        """
        :return: The total amount of columns
        """
        return self._cols


    def empty_cells(self):
        """
        Gets all empty cells in the board
        :return: All the empty cells
        """
        return [cell for row in self._board for cell in row if cell.is_empty()]

