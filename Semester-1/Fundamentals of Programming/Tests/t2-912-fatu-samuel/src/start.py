from src.ui.ui import UI
from src.service.service import Service
from src.repository.repository import Repository


repo = Repository()
service = Service(repo)
ui = UI(service)

ui.start()
