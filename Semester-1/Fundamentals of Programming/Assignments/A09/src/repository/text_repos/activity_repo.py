from src.repository.activity_repo import ActivityRepository
from src.domain.activity import Activity

from datetime import datetime


class ActivityTextRepository(ActivityRepository):

    def __init__(self, file_path):
        ActivityRepository.__init__(self)
        self.file_path = file_path
        self._load_file()


    def _load_file(self):
        with open(self.file_path, "r") as file:
            for line in file.readlines():
                args = line.split(maxsplit = 4, sep = ',')

                for i in range(5):
                    args[i] = args[i].strip()

                people = args[4][1:-1].split(sep = ',')
                for i in range(len(people)):
                    people[i] = int(people[i].strip())

                args[1] = args[1].replace('-', ' ')
                date = datetime.strptime(args[1], "%Y %m %d").date()
                time = datetime.strptime(args[2], "%H:%M:%S").time()

                ActivityRepository.add(self, Activity(int(args[0]), people, date, time, args[3]))


    def _save_file(self):
        activities = self.get_activities()
        with open(self.file_path, "w") as file:
            for activity in activities:
                file.write(f"{activity.id}, {activity.date}, {activity.time}, {activity.description}, {activity.people}\n")


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
