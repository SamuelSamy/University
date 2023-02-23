import json

from src.repository.person_repo import PersonRepository
from src.domain.person import Person

class PersonJsonRepository(PersonRepository):

    def __init__(self, file_path):
        PersonRepository.__init__(self)
        self.file_path = file_path
        self._load_file()


    def _load_file(self):
        with open(self.file_path) as file:
            data = json.load(file)

        for person_id in data.keys():
            name = data[person_id]["name"]
            number = data[person_id]["phone_number"]
            person = Person(int(person_id), name, number)
            PersonRepository.add(self, person)


    def _save_file(self):
        people = PersonRepository.get_people(self)
        data = {}
        for person in people:
            data[person.id] = {}
            data[person.id]["name"] = person.name
            data[person.id]["phone_number"] = person.phone_number

        with open(self.file_path, "w") as f:
            json.dump(data, f)


    def add(self, person):
        PersonRepository.add(self, person)
        self._save_file()


    def remove(self, person_id):
        PersonRepository.remove(self, person_id)
        self._save_file()


    def update(self, person_id, new_name, new_phone_number):
        PersonRepository.update(self, person_id, new_name, new_phone_number)
        self._save_file()
