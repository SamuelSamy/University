from ui.ui import UI
from ui.gui import GUI

from src.services.activity_service import ActivityService
from src.services.person_service import PersonService
from src.repository.activity_repo import ActivityRepository
from src.repository.person_repo import PersonRepository
from src.services.undo_service import UndoService

activity_repo = ActivityRepository()
person_repo = PersonRepository()

undo_service = UndoService()
activity_service = ActivityService(activity_repo, undo_service)
person_service = PersonService(activity_service, person_repo, undo_service)


UI_picked = False

while not UI_picked:
    print("1. Console UI")
    print("2. Graphical UI")
    option = input("How would you like to start the program? (Enter 1 or 2)\n")

    if option == '1':
        ui = UI(activity_service, person_service, undo_service)
        ui.main()
        UI_picked = True
    elif option == '2':
        gui = GUI(activity_service, person_service, undo_service)
        gui.activate_main_window()
        UI_picked = True
    else:
        print("Invalid option. Enter either 1 or 2")


