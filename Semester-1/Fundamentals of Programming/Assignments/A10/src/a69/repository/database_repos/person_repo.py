import sqlite3

from src.a69.repository.person_repo import PersonRepository
from src.a69.domain.person import Person


class PersonDatabaseRepository(PersonRepository):

    def __init__(self, file_path, table_name):

        # self.create_table()


        PersonRepository.__init__(self)
        self.file_path = file_path
        self.table_name = table_name
        self._load_file()


    def create_table(self):
        connection = sqlite3.connect(self.file_path)
        cursor = connection.cursor()
        cursor.execute('''create table people (ID int, name text, phone_number text)''')
        connection.commit()
        connection.close()


    def _load_file(self):

        connection = sqlite3.connect(self.file_path)
        connection.row_factory = sqlite3.Row
        cursor = connection.cursor()
        cursor.execute(f"select ID as id, name as name, phone_number as phone_number from {self.table_name}")
        rows = cursor.fetchall()

        for row in rows:
            PersonRepository.add(self, Person(int(row["id"]), row["name"], row["phone_number"]))

        connection.close()


    def add(self, person):
        PersonRepository.add(self, person)

        connection = sqlite3.connect(self.file_path)
        cursor = connection.cursor()
        cursor.execute(f"insert into {self.table_name} values (?, ?, ?)", (person.id, person.name, person.phone_number))
        connection.commit()
        connection.close()


    def remove(self, person_id):
        PersonRepository.remove(self, person_id)

        connection = sqlite3.connect(self.file_path)
        cursor = connection.cursor()
        cursor.execute(f"delete from {self.table_name} where ID = ?", (person_id,))
        connection.commit()
        connection.close()


    def update(self, person_id, new_name, new_phone_number):
        person = PersonRepository.update(self, person_id, new_name, new_phone_number)

        connection = sqlite3.connect(self.file_path)
        cursor = connection.cursor()
        cursor.execute(f"update {self.table_name} set name = ?, phone_number = ? where ID = ?", (person.name, person.phone_number, person_id))
        connection.commit()
        connection.close()
