import unittest

from src.services.person_service import PersonService
from src.services.activity_service import ActivityService
from src.repository.person_repo import PersonRepository
from src.repository.activity_repo import ActivityRepository
from src.services.serv_expections import FunctionCallException
from src.services.undo_service import UndoService
from datetime import date, time


class TestPersonRepo(unittest.TestCase):


    def test_add(self):

        repo = PersonRepository()
        undo_service = UndoService()
        activity_service = ActivityService(ActivityRepository(), undo_service)
        person_service = PersonService(activity_service, repo, undo_service)

        person_service.add(1, "Sam", "0701234567")



    def test_update(self):

        repo = PersonRepository()
        undo_service = UndoService()
        activity_service = ActivityService(ActivityRepository(), undo_service)
        person_service = PersonService(activity_service, repo, undo_service)
        person_service.add(1, "Sam", "0701234567")


        person_service.update(1, "Andi", "0712345678")



    def test_remove(self):

        repo = PersonRepository()
        undo_service = UndoService()
        activity_service = ActivityService(ActivityRepository(), undo_service)
        person_service = PersonService(activity_service, repo, undo_service)

        person_service.add(1, "Sam", "0701234567")

        activity_service.add(1, [1], date(2021, 11, 30), time(18, 0), "yes")
        activity_service.add(2, [1], date(2021, 11, 25), time(18, 0), "yes")
        activity_service.add(3, [2], date(2021, 11, 20), time(18, 0), "yes")


        person_service.remove(1)
        self.assertEqual(len(person_service.get_people()), 0)


        person_service.add(1, "Sam", "0701234567")


    def test_get_person_by_id(self):

        repo = PersonRepository()
        undo_service = UndoService()
        activity_service = ActivityService(ActivityRepository(), undo_service)
        person_service = PersonService(activity_service, repo, undo_service)
        person_service.add(1, "Andi", "0712345678")

        person = person_service.get_person_by_id(1)

        self.assertEqual(person.id, 1)
        self.assertEqual(person.name, "Andi")
        self.assertEqual(person.phone_number, "0712345678")


    def test_person_exists(self):

        repo = PersonRepository()
        undo_service = UndoService()
        activity_service = ActivityService(ActivityRepository(), undo_service)
        person_service = PersonService(activity_service, repo, undo_service)
        person_service.add(1, "Sam", "0701234567")

        self.assertEqual(person_service.person_exists(1), True)
        self.assertEqual(person_service.person_exists(2), False)


    def test_get_people_ids(self):

        repo = PersonRepository()
        undo_service = UndoService()
        activity_service = ActivityService(ActivityRepository(), undo_service)
        person_service = PersonService(activity_service, repo, undo_service)
        person_service.add(1, "Andi", "0712345678")

        people = person_service.get_people_ids()

        self.assertEqual(len(people), 1)


    def test_generate_people(self):

        repo = PersonRepository()
        undo_service = UndoService()
        activity_service = ActivityService(ActivityRepository(), undo_service)
        person_service = PersonService(activity_service, repo, undo_service)

        person_service.generate_people(5)

        self.assertEqual(len(person_service.get_people()), 5)

    def test_search_people(self):

        repo = PersonRepository()
        undo_service = UndoService()
        activity_service = ActivityService(ActivityRepository(), undo_service)
        person_service = PersonService(activity_service, repo, undo_service)

        person_service.add(1, "Sam", "0701234567")
        person_service.add(2, "Andi", "0701234567")
        person_service.add(3, "Test Samuel", "0704234567")

        people = person_service.search_people('sam', 'name')
        self.assertEqual(len(people), 2)

        people = person_service.search_people('01', 'number')
        self.assertEqual(len(people), 2)

        with self.assertRaises(FunctionCallException) as re:
            people = person_service.search_people('', 'name')

        self.assertEqual(str(re.exception), "The text can not be empty!")

        with self.assertRaises(FunctionCallException) as re:
            people = person_service.search_people('0701', 'yes')

        self.assertEqual(str(re.exception), "Search type must be either 'name' or 'number'")
