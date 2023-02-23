from ui.ui import UI
from ui.gui import GUI

from src.services.activity_service import ActivityService
from src.services.person_service import PersonService

from src.repository.activity_repo import ActivityRepository
from src.repository.person_repo import PersonRepository

from src.repository.text_repos.person_repo import PersonTextRepository
from src.repository.text_repos.activity_repo import ActivityTextRepository

from src.repository.binary_repos.person_repo import PersonBinaryRepository
from src.repository.binary_repos.activity_repo import ActivityBinaryRepository

from src.repository.json_repos.person_repo import PersonJsonRepository
from src.repository.json_repos.activity_repo import ActivityJsonRepository

from src.repository.database_repos.person_repo import PersonDatabaseRepository
from src.repository.database_repos.activity_repo import ActivityDatabaseRepository

from src.services.undo_service import UndoService
from src.settings.settings import Settings


activity_service = None
person_service = None
undo_service = None
generate = False

settings = Settings()


if settings.repository == 'in_memory':

    activity_repo = ActivityRepository()
    person_repo = PersonRepository()

    undo_service = UndoService()
    activity_service = ActivityService(activity_repo, undo_service)
    person_service = PersonService(activity_service, person_repo, undo_service)

    generate = True

elif settings.repository == 'text_files':

    activity_repo = ActivityTextRepository(f"files/{settings.activities}")
    person_repo = PersonTextRepository(f"files/{settings.people}")

    undo_service = UndoService()
    activity_service = ActivityService(activity_repo, undo_service)
    person_service = PersonService(activity_service, person_repo, undo_service)

    generate = False

elif settings.repository == 'binary_files':

    activity_repo = ActivityBinaryRepository(f"files/{settings.activities}")
    person_repo = PersonBinaryRepository(f"files/{settings.people}")

    undo_service = UndoService()
    activity_service = ActivityService(activity_repo, undo_service)
    person_service = PersonService(activity_service, person_repo, undo_service)

    generate = False

elif settings.repository == 'json_files':

    activity_repo = ActivityJsonRepository(f"files/{settings.activities}")
    person_repo = PersonJsonRepository(f"files/{settings.people}")

    undo_service = UndoService()
    activity_service = ActivityService(activity_repo, undo_service)
    person_service = PersonService(activity_service, person_repo, undo_service)

    generate = False

elif settings.repository == 'database':

    activity_repo = ActivityDatabaseRepository(f"files/database.db", settings.activities)
    person_repo = PersonDatabaseRepository(f"files/database.db", settings.people)

    undo_service = UndoService()
    activity_service = ActivityService(activity_repo, undo_service)
    person_service = PersonService(activity_service, person_repo, undo_service)

    generate = False

else:
    print("Invalid repository")
    exit()


if settings.ui == "ui":
    ui = UI(activity_service, person_service, undo_service, generate)
    ui.main()

elif settings.ui == "gui":
    gui = GUI(activity_service, person_service, undo_service, generate)
    gui.activate_main_window()

else:
    print("Invalid UI")


