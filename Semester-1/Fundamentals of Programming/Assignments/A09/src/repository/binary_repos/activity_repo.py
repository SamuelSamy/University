import os
import pickle

from src.repository.activity_repo import ActivityRepository


class ActivityBinaryRepository(ActivityRepository):

    def __init__(self, file_path):
        ActivityRepository.__init__(self)
        self.file_path = file_path
        self._load_file()


    def _load_file(self):
        if os.path.getsize(self.file_path) > 0:  # Check if the file is not empty
            with open(self.file_path, "rb") as file:
                self._data = pickle.load(file)


    def _save_file(self):
        with open(self.file_path, "wb") as file:
            pickle.dump(self._data, file)


    def add(self, activity):
        ActivityRepository.add(self, activity)
        self._save_file()


    def remove(self, activity_id):
        ActivityRepository.remove(self, activity_id)
        self._save_file()


    def update(self, activity_id, new_people_ids, new_date, new_time, new_description):
        ActivityRepository.update(self, activity_id, new_people_ids, new_date, new_time, new_description)
        self._save_file()


    def remove_person_from_activity(self, activity, person_id):
        ActivityRepository.remove_person_from_activity(self, activity, person_id)
        self._save_file()


    def add_person_to_activity(self, activity, person_id):
        ActivityRepository.add_person_to_activity(self, activity, person_id)
        self._save_file()
