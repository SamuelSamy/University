from prettytable import PrettyTable

from src.service.game_service import GameService


class TextColors:
    PURPLE = '\x1b[95m'
    BLUE = '\x1b[94m'
    GREEN = '\x1b[92m'
    YELLOW = '\x1b[93m'
    RED = '\x1b[91m'
    ENDC = '\x1b[0m'
    WHITE = ''
    BOLD = '\x1b[1m'
    UNDERLINE = '\x1b[4m'


class ConsoleUI:

    def __init__(self, service: GameService):

        self._service = service

        self._empty_cell_text = "o"

        self._human = self._service.human
        self._human_color = TextColors.RED
        self._human_text = "o"

        self._computer = self._service.computer
        self._computer_color = TextColors.YELLOW
        self._computer_text = "o"

    def start(self):

        self.display_board()

        winner = None
        player_turn = True

        while winner is None:

            if player_turn:
                col = self.get_player_input()
                self._service.place_piece(col, self._human, self._human_color, self._human_text)

            else:
                col = self._service.get_computer_move()
                print(f"Computer moves at: {col}")
                self._service.place_piece(col, self._computer, self._computer_color, self._computer_text)

            self.display_board()

            if self._service.was_winning_move():
                if player_turn:
                    winner = self._human
                else:
                    winner = self._computer

            elif self._service.is_draw():
                break

            player_turn = not player_turn

        if winner == self._human:
            print("Human wins")
        elif winner == self._computer:
            print("Computer wins")
        else:
            print("Draw")

    def display_board(self):

        table = PrettyTable()

        rows = self._service.get_total_rows()
        cols = self._service.get_total_cols()

        # Horizontal header
        header_row = []
        for i in range(cols):
            header_row.append(i)

        table.field_names = header_row

        for i in range(rows):

            _row = []

            for j in range(cols):
                cell = self._service.get_cell(i, j)
                piece = cell.piece

                if piece is None:
                    _row.append(f"{self._empty_cell_text}")
                else:
                    _row.append(f"{piece.color}{piece.display_text}{TextColors.ENDC}")

            table.add_row(_row)

        print(table)

    def get_player_input(self):

        valid = False
        while not valid:
            col = input("Enter a column: ")

            try:
                col = int(col)

                if col < 0 or col >= self._service.get_total_cols():
                    print("Invalid Input")

                elif not self._service.is_valid_column(col):
                    print("This column is full")
                else:
                    valid = True

            except ValueError:
                print("Invalid Input")

        return col
