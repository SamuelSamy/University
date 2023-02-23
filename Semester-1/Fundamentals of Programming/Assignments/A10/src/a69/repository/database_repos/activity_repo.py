import sqlite3

from src.a69.repository.activity_repo import ActivityRepository
from src.a69.domain.activity import Activity

from datetime import datetime


class ActivityDatabaseRepository(ActivityRepository):

    def __init__(self, file_path, table_name):

        # self.create_table()

        ActivityRepository.__init__(self)
        self.file_path = file_path
        self.table_name = table_name
        self._load_file()


    def create_table(self):
        connection = sqlite3.connect(self.file_path)
        cursor = connection.cursor()
        cursor.execute('''create table activities (ID int, date text, time text, description text, people text)''')
        connection.commit()
        connection.close()


    def _load_file(self):

        connection = sqlite3.connect(self.file_path)
        connection.row_factory = sqlite3.Row
        cursor = connection.cursor()
        cursor.execute(f"select ID as id, date as _date, time as _time, description as description, people as people from {self.table_name}")
        rows = cursor.fetchall()

        try:
            for row in rows:
                people = row["people"][1:-1].split(sep=',')
                for i in range(len(people)):
                    people[i] = int(people[i].strip())

                date = row["_date"].replace('-', ' ')
                date = datetime.strptime(date, "%Y %m %d").date()
                time = datetime.strptime(row["_time"], "%H:%M:%S").time()

                ActivityRepository.add(self, Activity(int(row["id"]), people, date, time, row["description"]))
        except:
            pass

        connection.close()


    def add(self, activity):
        ActivityRepository.add(self, activity)

        connection = sqlite3.connect(self.file_path)
        cursor = connection.cursor()
        cursor.execute(f"insert into {self.table_name} values (?, ?, ?, ?, ?)", (activity.id, str(activity.date), str(activity.time), str(activity.description), str(activity.people)))
        connection.commit()
        connection.close()


    def remove(self, activity_id):
        ActivityRepository.remove(self, activity_id)

        connection = sqlite3.connect(self.file_path)
        cursor = connection.cursor()
        cursor.execute(f"delete from {self.table_name} where ID = ?", (activity_id,))
        connection.commit()
        connection.close()


    def update(self, activity_id, new_people_ids, new_date, new_time, new_description):
        activity = ActivityRepository.update(self, activity_id, new_people_ids, new_date, new_time, new_description)

        connection = sqlite3.connect(self.file_path)
        cursor = connection.cursor()
        cursor.execute(f"update {self.table_name} set date = ?, time = ?, description = ?, people = ? where ID = ?",
                       (str(activity.date), str(activity.time), str(activity.description), str(activity.people), activity.id))
        connection.commit()
        connection.close()


    def remove_person_from_activity(self, activity, person_id):
        activity = ActivityRepository.remove_person_from_activity(self, activity, person_id)

        connection = sqlite3.connect(self.file_path)
        cursor = connection.cursor()
        cursor.execute(f"update {self.table_name} set date = ?, time = ?, description = ?, people = ? where ID = ?",
                       (str(activity.date), str(activity.time), str(activity.description), str(activity.people),
                        activity.id))
        connection.commit()
        connection.close()


    def add_person_to_activity(self, activity, person_id):
        activity = ActivityRepository.add_person_to_activity(self, activity, person_id)

        connection = sqlite3.connect(self.file_path)
        cursor = connection.cursor()
        cursor.execute(f"update {self.table_name} set date = ?, time = ?, description = ?, people = ? where ID = ?",
                       (str(activity.date), str(activity.time), str(activity.description), str(activity.people),
                        activity.id))
        connection.commit()
        connection.close()
