import os
import pickle

from src.repository.person_repo import PersonRepository


class PersonBinaryRepository(PersonRepository):

    def __init__(self, file_path):
        PersonRepository.__init__(self)
        self.file_path = file_path
        self._load_file()


    def _load_file(self):
        if os.path.getsize(self.file_path) > 0:  # Check if the file is not empty
            with open(self.file_path, "rb") as file:
                self._data = pickle.load(file)


    def _save_file(self):
        with open(self.file_path, "wb") as file:
            pickle.dump(self._data, file)


    def add(self, person):
        PersonRepository.add(self, person)
        self._save_file()


    def remove(self, person_id):
        PersonRepository.remove(self, person_id)
        self._save_file()


    def update(self, person_id, new_name, new_phone_number):
        PersonRepository.update(self, person_id, new_name, new_phone_number)
        self._save_file()
