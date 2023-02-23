from repository.game_repository import GameRepository
from service.game_service import GameService
from service.computer_service import Computer
from src.ui.gui import GUI
from ui.console import ConsoleUI


repo = GameRepository()
computer = Computer()
service = GameService(repo, computer, "Human", "Computer")


run_with_gui = True

if run_with_gui:
    gui = GUI(service)
    gui.start()
else:
    ui = ConsoleUI(service)
    ui.start()
