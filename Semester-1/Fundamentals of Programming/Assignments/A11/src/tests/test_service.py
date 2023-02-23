import copy
import unittest

from prettytable import PrettyTable

from src.domain.cell import Cell
from src.domain.piece import Piece
from src.service.game_service import GameService
from src.repository.game_repository import GameRepository
from src.service.computer_service import Computer


class GameTests(unittest.TestCase):

    def test_place_piece(self):

        repo = GameRepository()
        computer = Computer()
        service = GameService(repo, computer, "Human", "Computer")

        service.place_piece(0, "Human", "Red", "o")

        board = repo.get_copy_board()
        col = 0
        row = repo.get_next_row(col) + 1
        self.assertEqual(board[row][col].piece.owner, "Human")


    def test_is_valid_column(self):

        repo = GameRepository()
        computer = Computer()
        service = GameService(repo, computer, "Human", "Computer")

        self.assertEqual(service.is_valid_column(0), True)


        service.place_piece(0, "Human", "Red", "o")
        service.place_piece(0, "Human", "Red", "o")
        service.place_piece(0, "Human", "Red", "o")
        service.place_piece(0, "Human", "Red", "o")
        service.place_piece(0, "Human", "Red", "o")
        service.place_piece(0, "Human", "Red", "o")
        service.place_piece(0, "Human", "Red", "o")

        self.assertEqual(service.is_valid_column(0), False)


    def test_get_next_row(self):
        repo = GameRepository()
        computer = Computer()
        service = GameService(repo, computer, "Human", "Computer")

        service.place_piece(0, "Human", "Red", "o")

        row = repo.get_next_row(0)
        self.assertEqual(row, 4)


    def test_was_winning_move(self):

        computer = Computer()

        repo = GameRepository()
        service = GameService(repo, computer, "Human", "Computer")
        for i in range(4):
            service.place_piece(0, "Human", "Red", "o")

        self.assertEqual(service.was_winning_move(), "Human")

        repo = GameRepository()
        service = GameService(repo, computer, "Human", "Computer")
        for i in range(4):
            service.place_piece(0, "Computer", "Red", "o")

        self.assertEqual(service.was_winning_move(), "Computer")

        repo = GameRepository()
        service = GameService(repo, computer, "Human", "Computer")
        for i in range(4):
            service.place_piece(i, "Human", "Red", "o")

        self.assertEqual(service.was_winning_move(), "Human")

        repo = GameRepository()
        service = GameService(repo, computer, "Human", "Computer")
        for i in range(4):
            service.place_piece(i, "Computer", "Red", "o")

        self.assertEqual(service.was_winning_move(), "Computer")

        repo = GameRepository()
        service = GameService(repo, computer, "Human", "Computer")
        for i in range(4):
            for j in range(i + 1):
                service.place_piece(i, "Computer", "Red", "o")

        self.assertEqual(service.was_winning_move(), "Computer")

        repo = GameRepository()
        service = GameService(repo, computer, "Human", "Computer")
        for i in range(4):
            for j in range(i + 1):
                if j == i:
                    service.place_piece(i, "Human", "Red", "o")
                else:
                    service.place_piece(i, "Computer", "Red", "o")

        self.assertEqual(service.was_winning_move(), "Human")

        repo = GameRepository()
        service = GameService(repo, computer, "Human", "Computer")
        for i in range(6, 2, -1):
            for j in range(7 - i):
                if j == 6 - i:
                    service.place_piece(i, "Human", "Red", "x")
                else:
                    service.place_piece(i, "Computer", "Red", "-")

        self.assertEqual(service.was_winning_move(), "Human")

        repo = GameRepository()
        service = GameService(repo, computer, "Human", "Computer")
        self.assertEqual(service.was_winning_move(), None)

    def test_is_draw(self):

        repo = GameRepository()
        computer = Computer()
        service = GameService(repo, computer, "Human", "Computer")

        for i in range(service.get_total_cols()):
            for j in range(service.get_total_rows()):
                service.place_piece(i, "Human", "Red", "o")

        self.assertEqual(service.is_draw(), True)


    def test_get_matrix_location_by_mouse_position(self):

        repo = GameRepository()
        computer = Computer()
        service = GameService(repo, computer, "Human", "Computer")
        actual_col = service.get_matrix_location_by_mouse_position(50, 100)
        self.assertEqual(actual_col, (5, 0))

        actual_col = service.get_matrix_location_by_mouse_position(-50000, 100)
        self.assertEqual(actual_col, (5, 0))

        actual_col = service.get_matrix_location_by_mouse_position(50000, 100)
        self.assertEqual(actual_col, (5, 6))


    def test_get_cell(self):
        repo = GameRepository()
        computer = Computer()
        service = GameService(repo, computer, "Human", "Computer")
        cell = service.get_cell(0, 0)
        self.assertEqual(cell.row, 0)
        self.assertEqual(cell.col, 0)


    def test_get_computer_move(self):

        repo = GameRepository()
        computer = Computer()
        service = GameService(repo, computer, "Human", "Computer")

        service.place_piece(0, "Human", "Red", "o")
        service.place_piece(0, "Human", "Red", "o")
        service.place_piece(0, "Human", "Red", "o")
        col = service.get_computer_move()
        self.assertEqual(col, 0)

        service.place_piece(1, "Human", "Red", "o")
        service.place_piece(2, "Human", "Red", "o")
        col = service.get_computer_move()
        self.assertEqual(col, 00)


class ComputerTests(unittest.TestCase):

    def test_get_move(self):

        repo = GameRepository()
        computer = Computer()
        service = GameService(repo, computer, "Human", "Computer")

        service.place_piece(0, "Human", "Red", "X")

        copy_repo = copy.deepcopy(repo)
        col = computer.get_move(copy_repo, 3)[0]

        self.assertEqual(col, 3)

        repo = GameRepository()
        computer = Computer()
        service = GameService(repo, computer, "Human", "Computer")

        service.place_piece(1, "Human", "Red", "X")
        service.place_piece(2, "Human", "Red", "X")
        copy_repo = copy.deepcopy(repo)
        col = computer.get_move(copy_repo, 3)[0]

        self.assertEqual(col, 0)

        repo = GameRepository()
        computer = Computer()
        service = GameService(repo, computer, "Human", "Computer")

        service.place_piece(1, "Computer", "Red", "X")
        service.place_piece(2, "Computer", "Red", "X")
        service.place_piece(3, "Computer", "Red", "X")

        copy_repo = copy.deepcopy(repo)
        col = computer.get_move(copy_repo, 3)[0]

        self.assertEqual(col, 0)

        repo = GameRepository()
        computer = Computer()
        service = GameService(repo, computer, "Human", "Computer")

        service.place_piece(1, "Computer", "Red", "X")
        service.place_piece(2, "Computer", "Red", "X")

        copy_repo = copy.deepcopy(repo)
        col = computer.get_move(copy_repo, 3)[0]

        self.assertEqual(col, 0)

    def test_minimax(self):

        repo = GameRepository()
        computer = Computer()
        service = GameService(repo, computer, "Human", "Computer")

        for i in range(service.get_total_rows()):
            for j in range(service.get_total_cols()):
                service.place_piece(j, "Human", "Red", "X")

        copy_repo = copy.deepcopy(repo)
        col = computer.get_move(copy_repo, 3)[0]
        self.assertEqual(col, None)

        repo = GameRepository()
        computer = Computer()
        service = GameService(repo, computer, "Human", "Computer")

        repo._board[0][0].piece = Piece("Human", "Red", "'")
        repo._board[1][0].piece = Piece("Human", "Red", "'")
        repo._board[2][0].piece = Piece("Human", "Red", "'")
        repo._board[3][0].piece = Piece("Human", "Red", "'")


        copy_repo = copy.deepcopy(repo)
        col = computer.get_move(copy_repo, 3)[0]
        self.assertEqual(col, "Human")

        repo = GameRepository()
        computer = Computer()
        service = GameService(repo, computer, "Human", "Computer")

        repo._board[0][0].piece = Piece("Computer", "Red", "'")
        repo._board[1][0].piece = Piece("Computer", "Red", "'")
        repo._board[2][0].piece = Piece("Computer", "Red", "'")
        repo._board[3][0].piece = Piece("Computer", "Red", "'")

        copy_repo = copy.deepcopy(repo)
        col = computer.get_move(copy_repo, 3)[0]
        self.assertEqual(col, "Computer")


        cells = [Cell(0, 0) for x in range(4)]
        for cell in cells:
            piece = Piece("Computer", "Red", "x")
            cell.piece = piece

        self.assertEqual(computer.get_score_for_cells(cells), 100)

        cells = [Cell(0, 0) for x in range(4)]
        cells[0].piece = cells[1].piece = cells[2].piece = Piece("Human", "Red", "x")

        self.assertEqual(computer.get_score_for_cells(cells), -90)

    def test_was_winning_move(self):

        computer = Computer()

        repo = GameRepository()
        service = GameService(repo, computer, "Human", "Computer")
        for i in range(4):
            service.place_piece(0, "Human", "Red", "o")

        self.assertEqual(computer.was_winning_move(repo), "Human")

        repo = GameRepository()
        service = GameService(repo, computer, "Human", "Computer")
        for i in range(4):
            service.place_piece(0, "Computer", "Red", "o")

        self.assertEqual(computer.was_winning_move(repo), "Computer")

        repo = GameRepository()
        service = GameService(repo, computer, "Human", "Computer")
        for i in range(4):
            service.place_piece(i, "Human", "Red", "o")

        self.assertEqual(computer.was_winning_move(repo), "Human")

        repo = GameRepository()
        service = GameService(repo, computer, "Human", "Computer")
        for i in range(4):
            service.place_piece(i, "Computer", "Red", "o")

        self.assertEqual(computer.was_winning_move(repo), "Computer")

        repo = GameRepository()
        service = GameService(repo, computer, "Human", "Computer")
        for i in range(4):
            for j in range(i + 1):
                service.place_piece(i, "Computer", "Red", "o")

        self.assertEqual(computer.was_winning_move(repo), "Computer")

        repo = GameRepository()
        service = GameService(repo, computer, "Human", "Computer")
        for i in range(4):
            for j in range(i + 1):
                if j == i:
                    service.place_piece(i, "Human", "Red", "o")
                else:
                    service.place_piece(i, "Computer", "Red", "o")

        self.assertEqual(computer.was_winning_move(repo), "Human")

        repo = GameRepository()
        service = GameService(repo, computer, "Human", "Computer")
        for i in range(6, 2, -1):
            for j in range(7 - i):
                if j == 6 - i:
                    service.place_piece(i, "Human", "Red", "x")
                else:
                    service.place_piece(i, "Computer", "Red", "-")

        self.assertEqual(computer.was_winning_move(repo), "Human")

        repo = GameRepository()
        service = GameService(repo, computer, "Human", "Computer")
        self.assertEqual(computer.was_winning_move(repo), None)

