import copy
import math

from src.domain.exceptions import IndexOutOfMatrixBounds


class Computer:

    def __init__(self):

        self.human = "Human"
        self.computer = "Computer"


    def get_score_for_cells(self, cells):
        """
        Evaluates 4 cells in a row from the board

        - If there are 4 computer pieces in a row (the computer wins): score + 100
        - If there are 3 computer pieces in a row and 1 empty: score + 5
        - If there are 2 computer pieces in a row and 2 empty: score + 2
        - If there are 3 human pieces in a row and 1 empty: score - 90
        - If there are 2 computer pieces in a row and 2 empty at the margins: score - 50

        :param cells: The cells that we will check
        :return: The score
        """

        computer_pieces = len([cell for cell in cells if not cell.is_empty() and cell.piece.owner == self.computer])
        human_pieces = len([cell for cell in cells if not cell.is_empty() and cell.piece.owner == self.human])
        empty_pieces = 4 - computer_pieces - human_pieces

        score = 0

        if computer_pieces == 4:
            score += 100

        elif computer_pieces == 3 and empty_pieces == 1:
            score += 5

        elif computer_pieces == 2 and empty_pieces == 2:
            score += 2

        if human_pieces == 3 and empty_pieces == 1:
            score -= 90

        return score


    def evaluate_board(self, repo):
        """
        Evaluates the current state of the board
        :param repo: A `GameRepository` object
        :return: The total score
        """
        score = 0

        # horizontal
        for row in range(repo.get_total_rows()):
            for col in range(repo.get_total_cols() - 3):
                cells = [repo.get_cell(row, col + i) for i in range(4)]
                score += self.get_score_for_cells(cells)


        # vertical
        for row in range(repo.get_total_rows() - 3):
            for col in range(repo.get_total_cols()):
                cells = [repo.get_cell(row + i, col) for i in range(4)]
                score += self.get_score_for_cells(cells)


        # diagonals
        for row in range(repo.get_total_rows() - 3):
            for col in range(repo.get_total_cols() - 3):
                cells = [repo.get_cell(row + i, col + i) for i in range(4)]
                score += self.get_score_for_cells(cells)

                cells = [repo.get_cell(row + 3 - i, col + i) for i in range(4)]
                score += self.get_score_for_cells(cells)

        return score


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

    def was_winning_move(self, repo):
        """
        Checks if the last move was a winning one
        :return: The winner if there is one, False otherwise
        """
        # horizontal
        for row in range(repo.get_total_rows()):
            for col in range(repo.get_total_cols() - 3):
                cells = [repo.get_cell(row, col + i) for i in range(4)]

                winner = self.check_for_winner_in_4_cells(cells)
                if winner is not None:
                    return winner

        # vertical
        for row in range(repo.get_total_rows() - 3):
            for col in range(repo.get_total_cols()):
                cells = [repo.get_cell(row + i, col) for i in range(4)]

                winner = self.check_for_winner_in_4_cells(cells)
                if winner is not None:
                    return winner

        # diagonals
        for row in range(repo.get_total_rows() - 3):
            for col in range(repo.get_total_cols() - 3):

                cells = [repo.get_cell(row + i, col + i) for i in range(4)]

                winner = self.check_for_winner_in_4_cells(cells)
                if winner is not None:
                    return winner

                cells = [repo.get_cell(row + 3 - i, col + i) for i in range(4)]

                winner = self.check_for_winner_in_4_cells(cells)
                if winner is not None:
                    return winner

        return None


    def going_to_be_winning_move(self, cells):
        """
        Checks if there are 4 consecutive cells on the board where the first and last
        are empty and the middle ones have a piece in them
        :param cells: The 4 consecutive cells from the board
        :return: True if the above condition is true, False otherwise
        """
        if cells[0].is_empty() and cells[3].is_empty():
            if not cells[1].is_empty() and not cells[2].is_empty():
                if cells[1].piece.owner == self.computer and cells[1].piece.owner == self.computer or cells[1].piece.owner == self.human and cells[1].piece.owner == self.human:
                    return True

        return False



    def get_move(self, repo, depth, alpha = -math.inf, beta = math.inf, is_maximizing_player = True):
        """
        Gets the best move the computer can make
        - it checks if he can win in 1 move
        - it checks if the human can win in 1 move
        - it checks if he can have 2 consecutive pieces without neighbors
        - it checks if the human can have 2 consecutive pieces without neighbors
        If none of the above condition are met, the minimax function is called

        :param repo: A copy of the game repository
        :param depth: The current depth
        :param alpha: The alpha value for the minimax algorithm (-inf)
        :param beta: The beta value for the minimax algorithm (+inf)
        :param is_maximizing_player: True by default
        :return: The best move the computer can make
        """
        winner = self.was_winning_move(repo)

        if winner is None:
            for col in range(repo.get_total_cols()):

                if repo.is_valid_column(col):
                    copy_repo = copy.deepcopy(repo)
                    copy_repo.place_piece(col, self.computer)

                    if self.was_winning_move(copy_repo) == self.computer:
                        return col, 0

                    copy_repo = copy.deepcopy(repo)
                    copy_repo.place_piece(col, self.human)

                    if self.was_winning_move(copy_repo) == self.human:
                        return col, 0

        # horizontal
        for row in range(repo.get_total_rows()):
            for col in range(repo.get_total_cols() - 3):
                cells = [repo.get_cell(row, col + i) for i in range(4)]
                if self.going_to_be_winning_move(cells):
                    return col, 0


        # # vertical
        # for row in range(repo.get_total_rows() - 3):
        #     for col in range(repo.get_total_cols()):
        #         cells = [repo.get_cell(row + i, col) for i in range(4)]
        #         if self.going_to_be_winning_move(cells):
        #             return col, 0


        # diagonals
        # for row in range(repo.get_total_rows() - 3):
        #     for col in range(repo.get_total_cols() - 3):
        #         cells = [repo.get_cell(row + i, col + i) for i in range(4)]
        #
        #         if self.going_to_be_winning_move(cells):
        #             return col, 0
        #
        #         cells = [repo.get_cell(row + 3 - i, col + i) for i in range(4)]
        #         if self.going_to_be_winning_move(cells):
        #             return col, 0


        return self.minimax(repo, depth, alpha, beta, is_maximizing_player)


    def minimax(self, repo, depth, alpha, beta, is_maximizing_player):
        """
        The minimax algorithm
        It minimizes the possible lose for a worst case scenario

        :param repo: A copy of the game repository
        :param depth: The current depth
        :param alpha: The alpha value
        :param beta: The beta value
        :param is_maximizing_player: True if its maximizing player, False otherwise
        :return: The best move the computer can make
        """

        if len(repo.empty_cells()) == 0:
            return None, 0

        winner = self.was_winning_move(repo)
        if winner == self.human:
            return self.human, -100000000000000000
        elif winner == self.computer:
            return self.computer, 100000000000000000

        if depth == 0:
            return None, self.evaluate_board(repo)

        if is_maximizing_player:

            best_score = -math.inf
            column = None

            for col in range(repo.get_total_cols()):
                if repo.is_valid_column(col):

                    copy_repo = copy.deepcopy(repo)
                    copy_repo.place_piece(col, self.computer)
                    new_score = self.minimax(copy_repo, depth - 1, alpha, beta, False)[1]

                    if new_score > best_score:
                        best_score = new_score
                        column = col

                    alpha = max(alpha, best_score)
                    if alpha >= beta:
                        break

            return column, best_score - depth

        else:

            best_score = math.inf
            column = None

            for col in range(repo.get_total_cols()):
                if repo.is_valid_column(col):

                    copy_repo = copy.deepcopy(repo)
                    copy_repo.place_piece(col, self.computer)
                    new_score = self.minimax(copy_repo, depth - 1, alpha, beta, True)[1]

                    if new_score < best_score:
                        best_score = new_score
                        column = col

                    beta = min(beta, best_score)
                    if alpha >= beta:
                        break

            return column, best_score + depth
