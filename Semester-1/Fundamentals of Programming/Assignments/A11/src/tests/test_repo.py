import unittest

from src.domain.exceptions import IndexOutOfMatrixBounds
from src.repository.game_repository import GameRepository


class RepoTests(unittest.TestCase):


    def test_get_copy_board(self):

        repo = GameRepository()
        repo.place_piece(0, "Human", "Red", "o")

        board = repo.get_copy_board()

        for i in range(repo.get_total_rows()):
            for j in range(repo.get_total_cols()):
                self.assertEqual(str(board[i][j]), str(repo._board[i][j]))


    def test_place_piece(self):

        repo = GameRepository()

        repo.place_piece(0, "Human", "Red", "o")

        board = repo.get_copy_board()
        col = 0
        row = repo.get_next_row(col) + 1
        self.assertEqual(board[row][col].piece.owner, "Human")


    def test_is_valid_column(self):

        repo = GameRepository()

        self.assertEqual(repo.is_valid_column(0), True)

        repo.place_piece(0, "Human", "Red", "o")
        repo.place_piece(0, "Human", "Red", "o")
        repo.place_piece(0, "Human", "Red", "o")
        repo.place_piece(0, "Human", "Red", "o")
        repo.place_piece(0, "Human", "Red", "o")
        repo.place_piece(0, "Human", "Red", "o")
        repo.place_piece(0, "Human", "Red", "o")

        self.assertEqual(repo.is_valid_column(0), False)


    def test_get_next_row(self):
        repo = GameRepository()
        repo.place_piece(0, "Human", "Red", "o")
        row = repo.get_next_row(0)
        self.assertEqual(row, 4)


    def test_get_cell(self):
        repo = GameRepository()
        cell = repo.get_cell(0, 0)
        self.assertEqual(cell.row, 0)
        self.assertEqual(cell.col, 0)

        with self.assertRaises(IndexOutOfMatrixBounds):
            cell = repo.get_cell(-1, -1)


    def test_is_valid_position(self):

        repo = GameRepository()
        self.assertEqual(repo.is_valid_position(-5, -5), False)
        self.assertEqual(repo.is_valid_position(0, 0), True)


    def test_get_total_rows(self):
        repo = GameRepository()
        self.assertEqual(repo.get_total_rows(), 6)

    def test_get_total_cols(self):
        repo = GameRepository()
        self.assertEqual(repo.get_total_cols(), 7)

    def test_empty_cells(self):
        repo = GameRepository()
        value = len(repo.empty_cells())
        expected = repo.get_total_cols() * repo.get_total_cols() - repo.get_total_cols()
        self.assertEqual(value, expected)
        repo.place_piece(0, "Human", "Red", "o")
        repo.place_piece(0, "Human", "Red", "o")

        self.assertEqual(len(repo.empty_cells()), repo.get_total_cols() * repo.get_total_cols() - 2 - repo.get_total_cols())
