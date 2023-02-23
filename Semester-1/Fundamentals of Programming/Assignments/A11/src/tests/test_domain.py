import unittest

from src.domain.cell import Cell
from src.domain.exceptions import ConnectException
from src.domain.piece import Piece


class DomainTests(unittest.TestCase):


    def test_cell_set_piece(self):

        cell = Cell(0, 0)
        piece = Piece("Human", "Red", "x")
        cell.piece = piece
        self.assertEqual(cell.piece, piece)

        with self.assertRaises(ConnectException):
            cell.piece = 0


    def test_piece_color(self):
        piece = Piece("Human", "Red", "x")
        self.assertEqual(piece.color, "Red")

    def test_display_text(self):
        piece = Piece("Human", "Red", "x")
        self.assertEqual(piece.display_text, "x")

    def test_eq(self):
        piece = Piece("Human", "Red", "x")
        self.assertEqual(piece == 1, False)
