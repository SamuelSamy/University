import unittest

from src.a69.services.person_service import PersonService
from src.a69.services.activity_service import ActivityService
from src.a69.repository.person_repo import PersonRepository
from src.a69.repository.activity_repo import ActivityRepository
from src.a69.services.serv_expections import FunctionCallException
from src.a69.services.undo_service import UndoService
from src.a69.services.serv_expections import UndoException

from datetime import date, time


class TestPersonRepo(unittest.TestCase):

    def test_add(self):
        person_repo = PersonRepository()
        activity_repo = ActivityRepository()

        undo_service = UndoService()

        activity_service = ActivityService(activity_repo, undo_service)
        person_service = PersonService(activity_service, person_repo, undo_service)

        person_service.add(1, "Sam", "0712345678")
        self.assertEqual(len(person_service.get_people()), 1)

        undo_service.undo()
        self.assertEqual(len(person_service.get_people()), 0)

        undo_service.redo()
        self.assertEqual(len(person_service.get_people()), 1)


    def test_remove_person(self):

        person_repo = PersonRepository()
        activity_repo = ActivityRepository()

        undo_service = UndoService()

        activity_service = ActivityService(activity_repo, undo_service)
        person_service = PersonService(activity_service, person_repo, undo_service)

        person_service.add(1, "Sam", "0712345678")
        activity_service.add(1, [1], date(2021, 12, 1), time(18, 0), "Testing")

        person_service.remove(1)

        activity = activity_service.get_activity_by_id(1)
        self.assertEqual(len(person_service.get_people()), 0)
        self.assertEqual(len(activity.people), 0)

        undo_service.undo()

        self.assertEqual(len(person_service.get_people()), 1)
        self.assertEqual(len(activity.people), 1)


        undo_service.redo()

        self.assertEqual(len(person_service.get_people()), 0)
        self.assertEqual(len(activity.people), 0)


    def test_update_person(self):

        person_repo = PersonRepository()
        activity_repo = ActivityRepository()

        undo_service = UndoService()

        activity_service = ActivityService(activity_repo, undo_service)
        person_service = PersonService(activity_service, person_repo, undo_service)

        person_service.add(1, "Sam", "0712345678")

        person_service.update(1, "Test", "0787654321")

        undo_service.undo()

        person = person_service.get_person_by_id(1)
        self.assertEqual(person.name, "Sam")
        self.assertEqual(person.phone_number, "0712345678")

        undo_service.redo()

        person = person_service.get_person_by_id(1)
        self.assertEqual(person.name, "Test")
        self.assertEqual(person.phone_number, "0787654321")


    def test_add_activity(self):

        activity_repo = ActivityRepository()

        undo_service = UndoService()

        activity_service = ActivityService(activity_repo, undo_service)

        activity_service.add(1, [], date(2021, 12, 1), time(18), "Testing")
        self.assertEqual(len(activity_service.get_activities()), 1)

        undo_service.undo()
        self.assertEqual(len(activity_service.get_activities()), 0)

        undo_service.redo()
        self.assertEqual(len(activity_service.get_activities()), 1)


    def test_remove_activity(self):

        activity_repo = ActivityRepository()

        undo_service = UndoService()

        activity_service = ActivityService(activity_repo, undo_service)

        activity_service.add(1, [], date(2021, 12, 1), time(18), "Testing")

        activity_service.remove(1)
        self.assertEqual(len(activity_service.get_activities()), 0)

        undo_service.undo()
        self.assertEqual(len(activity_service.get_activities()), 1)

        undo_service.redo()
        self.assertEqual(len(activity_service.get_activities()), 0)


    def test_update_activity(self):

        activity_repo = ActivityRepository()

        undo_service = UndoService()

        activity_service = ActivityService(activity_repo, undo_service)

        activity_service.add(1, [], date(2021, 12, 1), time(18), "Testing")

        activity_service.update(1, [1], date(2022, 12, 1), time(20), "Yes")
        activity = activity_service.get_activity_by_id(1)
        self.assertEqual(activity.people, [1])
        self.assertEqual(activity.date, date(2022, 12, 1))
        self.assertEqual(activity.time, time(20))
        self.assertEqual(activity.description, "Yes")

        undo_service.undo()
        activity = activity_service.get_activity_by_id(1)
        self.assertEqual(activity.people, [])
        self.assertEqual(activity.date, date(2021, 12, 1))
        self.assertEqual(activity.time, time(18))
        self.assertEqual(activity.description, "Testing")


        undo_service.redo()
        activity = activity_service.get_activity_by_id(1)
        self.assertEqual(activity.people, [1])
        self.assertEqual(activity.date, date(2022, 12, 1))
        self.assertEqual(activity.time, time(20))
        self.assertEqual(activity.description, "Yes")


    def test_exceptions(self):

        person_repo = PersonRepository()
        activity_repo = ActivityRepository()

        undo_service = UndoService()

        activity_service = ActivityService(activity_repo, undo_service)
        person_service = PersonService(activity_service, person_repo, undo_service)

        with self.assertRaises(UndoException) as ue:
            undo_service.undo()

        self.assertEqual(str(ue.exception), "Nothing left to undo")

        with self.assertRaises(UndoException) as ue:
            undo_service.redo()

        self.assertEqual(str(ue.exception), "Nothing left to redo")
