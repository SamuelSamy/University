import json

from src.a69.repository.activity_repo import ActivityRepository
from src.a69.domain.activity import Activity

from datetime import datetime


class ActivityJsonRepository(ActivityRepository):

    def __init__(self, file_path):
        ActivityRepository.__init__(self)
        self.file_path = file_path
        self._load_file()


    def _load_file(self):
        with open(self.file_path) as file:
            data = json.load(file)

        for activity_id in data.keys():
            date = data[activity_id]["date"].replace('-', ' ')
            date = datetime.strptime(date, "%Y %m %d").date()
            time = datetime.strptime(data[activity_id]["time"], "%H:%M:%S").time()
            people = data[activity_id]["people"]
            for i in range(len(people)):
                people[i] = int(people[i])

            activity = Activity(int(activity_id), people, date, time, data[activity_id]["description"])
            ActivityRepository.add(self, activity)


    def _save_file(self):
        activities = ActivityRepository.get_activities(self)
        data = {}
        for activity in activities:
            data[activity.id] = {}
            data[activity.id]["date"] = str(activity.date)
            data[activity.id]["time"] = str(activity.time)
            data[activity.id]["description"] = activity.description
            data[activity.id]["people"] = activity.people

        with open(self.file_path, "w") as f:
            json.dump(data, f)


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
