from ui.ui import UI
from service.service import Service
from repository.repository import Repository

repository = Repository()
service = Service(repository)
ui = UI(service)

if __name__ == '__main__':
    ui.main()
