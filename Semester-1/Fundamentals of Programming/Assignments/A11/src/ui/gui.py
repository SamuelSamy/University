import pygame
import time

from src.service.game_service import GameService


class Colors:

    RED = "#ed0915"
    PINK = "#ed092f"
    YELLOW = "#e5f507"
    BLUE = "#3d2cf5"
    BLACK = "#1b1b1c"


class GUI:

    def __init__(self, service: GameService):
        self._service = service

        self._human = self._service.human
        self._human_color = Colors.RED
        self._human_text = "o"

        self._computer = self._service.computer
        self._computer_color = Colors.YELLOW
        self._computer_text = "o"

        self.__init__pygame()


    def __init__pygame(self):

        self.__last_drawn_premove = None

        self.__square_size = 100
        self.__radius = int(self.__square_size / 2) - 5

        self.__rows = self._service.get_total_rows()
        self.__cols = self._service.get_total_cols()

        self.__window_height = self.__rows * self.__square_size
        self.__window_width = self.__cols * self.__square_size

        pygame.init()
        self.__canvas = pygame.display.set_mode([self.__window_width, self.__window_height + 100])

        pygame.display.set_caption("Connect 4")


    def start(self):

        game_is_running = True
        winner = None
        self._draw_canvas()


        while True:


            for event in pygame.event.get():

                if event.type == pygame.QUIT:
                    return

                if event.type == pygame.MOUSEMOTION and game_is_running:
                    self._draw_premove(event)


                if event.type == pygame.MOUSEBUTTONDOWN and game_is_running:

                    row, col = self._service.get_matrix_location_by_mouse_position(event.pos[0], self.__square_size)

                    if self._service.is_valid_column(col):
                        self._service.place_piece(col, self._human, self._human_color, self._human_text)

                        self._draw_canvas()

                        if self._service.was_winning_move():
                            winner = self._human
                            game_is_running = False

                        else:

                            col = self._service.get_computer_move()
                            self._service.place_piece(col, self._computer, self._computer_color, self._computer_text)

                            self._draw_canvas()

                            if self._service.was_winning_move():
                                winner = self._computer
                                game_is_running = False

                        if game_is_running and self._service.is_draw():
                            winner = None
                            game_is_running = False

                        self._draw_canvas()

                        if game_is_running:
                            self._draw_premove(event)
                        else:
                            text = "It's a draw!"

                            if winner == self._human:
                                text = "You win!"
                            elif winner == self._computer:
                                text = "You lose!"

                            self.draw_end_screen(text)


    def draw_end_screen(self, text):

        pygame.draw.rect(self.__canvas, Colors.BLUE, (0, 0, self.__cols * self.__square_size, self.__square_size))
        font = pygame.font.SysFont("monospace", 72)
        text = font.render(text, True, Colors.BLACK)
        text_rect = text.get_rect()
        location = ((self.__window_width - text_rect[2]) / 2, (self.__square_size - text_rect[3]) / 2)
        self.__canvas.blit(text, location)

        pygame.display.update()


    def _draw_premove(self, event):

        for col in range(self.__cols):
            if self._service.is_valid_column(col):
                row = self._service.get_next_row(col)
                cell = self._service.get_cell(row, col)
                if cell.is_empty():
                    last_actual_location = (col * self.__square_size + int(self.__square_size / 2), (row + 1) * self.__square_size + int(self.__square_size / 2))
                    pygame.draw.circle(self.__canvas, Colors.BLACK, last_actual_location, self.__radius)

        location = self._service.get_matrix_location_by_mouse_position(event.pos[0], self.__square_size)

        if self._service.is_valid_column(location[1]):

            actual_location = (location[1] * self.__square_size + int(self.__square_size / 2), (location[0] + 1) * self.__square_size + int(self.__square_size / 2))
            pygame.draw.circle(self.__canvas, Colors.PINK, actual_location, self.__radius)

        pygame.display.update()


    def _draw_canvas(self):

        self.__canvas.fill(Colors.BLUE)

        for i in range(self.__rows):
            for j in range(self.__cols):

                cell = self._service.get_cell(i, j)

                if cell.is_empty():
                    color = Colors.BLACK
                else:
                    if cell.piece.owner == self._human:
                        color = Colors.RED
                    else:
                        color = Colors.YELLOW

                location = (int(j * self.__square_size + self.__square_size / 2), int((i + 1) * self.__square_size + self.__square_size / 2))
                pygame.draw.circle(self.__canvas, color, location, self.__radius)

        self.draw_end_screen("Connect 4")
        pygame.display.update()
