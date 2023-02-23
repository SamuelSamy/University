import unittest

from src.domain.person import Person
from src.repository.repo_exceptions import RepositoryException
from src.repository.person_repo import PersonRepository


class TestPersonRepo(unittest.TestCase):


    def test_add(self):

        repo = PersonRepository()

        repo.add(Person(1, "Sam", "0701234567"))

        with self.assertRaises(RepositoryException) as re:
            repo.add(Person(1, "Damn", "0701234568"))

        self.assertEqual(str(re.exception), "Duplicate Person ID")


    def test_update(self):

        repo = PersonRepository()
        repo.add(Person(1, "Sam", "0701234567"))


        repo.update(1, "Andi", "0787654321")

        with self.assertRaises(RepositoryException) as re:
            repo.update(2, "Andi", "0712345678")

        self.assertEqual(str(re.exception), "No person found with the specified ID")


    def test_get_person_by_id(self):

        repo = PersonRepository()
        repo.add(Person(1, "Andi", "0787654321"))


        person = repo.get_person_by_id(1)
        self.assertEqual(person.id, 1)
        self.assertEqual(person.name, "Andi")
        self.assertEqual(person.phone_number, "0787654321")

        with self.assertRaises(RepositoryException) as re:
            person = repo.get_person_by_id(2)

        self.assertEqual(str(re.exception), "No person with the specified ID found")


    def test_person_exists(self):

        repo = PersonRepository()
        repo.add(Person(1, "Andi", "0787654321"))

        self.assertEqual(repo.person_exists(1), True)
        self.assertEqual(repo.person_exists(10), False)


    def test_remove(self):

        repo = PersonRepository()
        repo.add(Person(1, "Sam", "0701234567"))
        
        with self.assertRaises(RepositoryException) as re:
            repo.remove(2)

        self.assertEqual(str(re.exception), "No person found with the specified ID")

        repo.remove(1)


    def test_get_people(self):

        repo = PersonRepository()
        repo.add(Person(1, "Sam", "0701234567"))

        people = repo.get_people()
        self.assertEqual(len(people), 1)


    def test_get_people_ids(self):

        repo = PersonRepository()
        repo.add(Person(1, "Sam", "0701234567"))

        people = repo.get_people_ids()
        self.assertEqual(len(people), 1)
