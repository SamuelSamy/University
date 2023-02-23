import copy
import math

from src.domain.exceptions import IndexOutOfMatrixBounds
from src.repository.game_repository import GameRepository
from src.service.computer_service import Computer

class GameService:

    def __init__(self, repository: GameRepository, computer_service: Computer, human, computer):
        self.__repository = repository

        self.human = human
        self.computer = computer

        self._computer = computer_service
        self._computer.human = human
        self._computer.computer = computer


    def place_piece(self, col, piece_owner, piece_color = None, piece_text = None):
        """
        Place a piece in the specified column
        :param col: The column in which the piece is going to be placed
        :param piece_owner: The owner of the piece
        :param piece_color: A (string) color representing the piece color (Used for console)
        :param piece_text:A string representing the piece text (Used for console)
        :return: None
        """
        self.__repository.place_piece(col, piece_owner, piece_color, piece_text)


    def is_valid_column(self, col):
        """
        Checks if any piece can be placed on the specified column
        :param col: The column we want to check
        :return: True, if we can place the piece, False otherwise
        """
        return self.__repository.is_valid_column(col)


    def get_next_row(self, col):
        """
        Gets the next available row in the specified column
        :param col: The column we want to check
        :return: -1 if the column is full, otherwise the row index
        """
        return self.__repository.get_next_row(col)


    def get_cell(self, row, col):
        """
        Gets a `Cell` object by its index in the board
        :param row: The row in which the cell is in
        :param col: The column in which the cell is in
        :return: A `Cell` Object
        """
        return self.__repository.get_cell(row, col)


    def get_total_rows(self):
        """
        :return: The total amount of rows
        """
        return self.__repository.get_total_rows()


    def get_total_cols(self):
        """
        :return: The total amount of columns
        """
        return self.__repository.get_total_cols()


    def check_for_winner_in_4_cells(self, cells):
        """
        :param cells: A list containing 4 `Cell` objects
        :return: The winner if there is one, None otherwise
        """
        computer_pieces = len([cell for cell in cells if not cell.is_empty() and cell.piece.owner == self.computer])
        human_pieces = len([cell for cell in cells if not cell.is_empty() and cell.piece.owner == self.human])

        if computer_pieces == 4:
            return self.computer

        if human_pieces == 4:
            return self.human

        return None


    def was_winning_move(self):
        """
        Checks if the last move was a winning one
        :return: The winner if there is one, False otherwise
        """
        # horizontal
        for row in range(self.__repository.get_total_rows()):
            for col in range(self.__repository.get_total_cols() - 3):
                cells = [self.__repository.get_cell(row, col + i) for i in range(4)]

                winner = self.check_for_winner_in_4_cells(cells)
                if winner is not None:
                    return winner

        # vertical
        for row in range(self.__repository.get_total_rows() - 3):
            for col in range(self.__repository.get_total_cols()):
                cells = [self.__repository.get_cell(row + i, col) for i in range(4)]

                winner = self.check_for_winner_in_4_cells(cells)
                if winner is not None:
                    return winner

        # diagonals
        for row in range(self.__repository.get_total_rows() - 3):
            for col in range(self.__repository.get_total_cols() - 3):

                cells = [self.__repository.get_cell(row + i, col + i) for i in range(4)]

                winner = self.check_for_winner_in_4_cells(cells)
                if winner is not None:
                    return winner


                cells = [self.__repository.get_cell(row + 3 - i, col + i) for i in range(4)]

                winner = self.check_for_winner_in_4_cells(cells)
                if winner is not None:
                    return winner

        return None


    def is_draw(self):
        """
        :return: True if there are no more empty cells on the board, False otherwise
        """
        return len(self.__repository.empty_cells()) == 0


    def get_matrix_location_by_mouse_position(self, mouse_position_x, square_size):
        """
        Gets the actual column based on the mouse's position
        :param mouse_position_x: The position the mouse is at
        :param square_size: The size of a column
        :return: The row and column the piece has the be placed at
        """
        total_cols = self.get_total_cols()

        actual_col = int(mouse_position_x / square_size)

        if actual_col < 0:
            actual_col = 0

        if actual_col >= total_cols:
            actual_col = total_cols - 1

        row = self.get_next_row(actual_col)

        return row, actual_col



    # Computer Service


    def get_computer_move(self):
        """
        Gets the best move the computer can do
        :return: The column the computer will place the piece
        """
        copy_repo = copy.deepcopy(self.__repository)
        data = self._computer.get_move(copy_repo, 5)
        return data[0]



