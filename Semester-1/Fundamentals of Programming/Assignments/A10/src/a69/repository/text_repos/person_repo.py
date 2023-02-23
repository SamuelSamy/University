from src.a69.repository.person_repo import PersonRepository
from src.a69.domain.person import Person


class PersonTextRepository(PersonRepository):

    def __init__(self, file_path):
        PersonRepository.__init__(self)
        self.file_path = file_path
        self._load_file()

    def _load_file(self):
        with open(self.file_path, "r") as file:
            for line in file.readlines():
                args = line.split(maxsplit = 2, sep = ',')

                for i in range(3):
                    args[i] = args[i].strip()

                PersonRepository.add(self, Person(int(args[0]), args[1], args[2]))


    def _save_file(self):
        people = PersonRepository.get_people(self)
        with open(self.file_path, "w") as file:
            for person in people:
                file.write(f"{person.id}, {person.name}, {person.phone_number}\n")


    def add(self, person):
        PersonRepository.add(self, person)
        self._save_file()


    def remove(self, person_id):
        PersonRepository.remove(self, person_id)
        self._save_file()


    def update(self, person_id, new_name, new_phone_number):
        PersonRepository.update(self, person_id, new_name, new_phone_number)
        self._save_file()
