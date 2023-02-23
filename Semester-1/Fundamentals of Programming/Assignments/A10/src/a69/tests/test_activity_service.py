import unittest

from src.a69.repository.activity_repo import ActivityRepository
from src.a69.services.activity_service import ActivityService
from src.a69.services.serv_expections import FunctionCallException
from src.a69.services.undo_service import UndoService
from src.a69.repository.repo_exceptions import RepositoryException
from datetime import date, time


class TestActivityService(unittest.TestCase):

    def test_add(self):

        repo = ActivityRepository()
        undo_service = UndoService()
        service = ActivityService(repo, undo_service)


        service.add(1, [1, 2], date(2021, 11, 20), time(18, 0, 0), "Test")

        with self.assertRaises(RepositoryException) as re:
            service.add(1, [1, 2, 3], date(2021, 12, 20), time(20, 0, 0), "Test")

        self.assertEqual(str(re.exception), "Duplicate Activity ID")


    def test_get_activities(self):

        repo = ActivityRepository()
        undo_service = UndoService()
        service = ActivityService(repo, undo_service)

        service.add(1, [1, 2], date(2021, 11, 20), time(18, 0, 0), "Test")


        activities = service.get_activities()
        self.assertEqual(len(activities), 1)



    def test_update(self):
        repo = ActivityRepository()
        undo_service = UndoService()
        service = ActivityService(repo, undo_service)

        service.add(1, [1, 2], date(2021, 11, 20), time(18, 0, 0), "Test")

        service.update(1, [1], date(2021, 12, 20), time(20, 0, 0), "Yes")


    def test_remove(self):
        repo = ActivityRepository()
        undo_service = UndoService()
        service = ActivityService(repo, undo_service)

        service.add(1, [1, 2], date(2021, 11, 20), time(18, 0, 0), "Test")


        service.remove(1)

        service.add(1, [1, 2], date(2021, 11, 20), time(18, 0, 0), "Test")

        activity = service.get_activity_by_id(1)


    def test_generate_activities(self):

        repo = ActivityRepository()
        undo_service = UndoService()
        service = ActivityService(repo, undo_service)

        service.generate_activities(10, [1, 2, 3, 4])

        self.assertEqual(len(service.get_activities()), 10)


    def test_remove_person_from_activity(self):

        repo = ActivityRepository()
        undo_service = UndoService()
        service = ActivityService(repo, undo_service)
        service.add(1, [1, 2], date(2021, 11, 20), time(18, 0), "yes")

        activity = service.get_activity_by_id(1)

        service.remove_person_from_activity(activity, 1)

        self.assertEqual(len(service.get_people_ids_in_activity(1)), 1)


    def test_get_activities_by_date(self):

        repo = ActivityRepository()
        undo_service = UndoService()
        service = ActivityService(repo, undo_service)
        service.add(1, [1, 2], date(2021, 11, 20), time(18, 0, 0), "Very Cool Activity")
        service.add(2, [2, 2], date(2021, 12, 20), time(18, 0, 0), "Test")

        activities = service.get_activities_by_date(date(2021, 11, 20))
        self.assertEqual(len(activities), 1)

        with self.assertRaises(FunctionCallException) as re:
            activities = service.get_activities_by_date("2021-11-20")

        self.assertEqual(str(re.exception), "date type must be `datetime.date`")


    def test_get_busiest_days(self):

        repo = ActivityRepository()
        undo_service = UndoService()
        service = ActivityService(repo, undo_service)
        service.add(1, [1, 2], date(2021, 11, 20), time(18, 0, 0), "Very Cool Activity")
        service.add(2, [1, 2], date(2021, 11, 20), time(10, 0, 0), "Very Cool Activity")

        days = service.get_busiest_days()
        self.assertEqual(len(days), 1)
        self.assertEqual(days[0][0], '2021-11-20')
        self.assertEqual(days[0][1], 2)


    def test_get_activities_with_person(self):

        repo = ActivityRepository()
        undo_service = UndoService()
        service = ActivityService(repo, undo_service)
        service.add(1, [1, 2], date(2021, 11, 20), time(18, 0, 0), "Very Cool Activity")
        service.add(2, [2], date(2021, 12, 20), time(18, 0, 0), "Very Cool Activity")

        activities = service.get_activities_with_person(1)
        self.assertEqual(len(activities), 1)


    def test_search_activity(self):

        repo = ActivityRepository()
        undo_service = UndoService()
        service = ActivityService(repo, undo_service)
        service.add(1, [1, 2], date(2021, 11, 20), time(18, 0, 0), "Very Cool Activity")
        service.add(2, [2], date(2021, 12, 20), time(20, 0, 0), "Test")

        activities = service.search_activity("Very", 'description')
        self.assertEqual(len(activities), 1)

        activities = service.search_activity("11", 'date')
        self.assertEqual(len(activities), 1)

        activities = service.search_activity("18", 'time')
        self.assertEqual(len(activities), 1)

        with self.assertRaises(FunctionCallException) as re:
            activities = service.search_activity("", 'time')

        self.assertEqual(str(re.exception), "The text can not be empty!")

        with self.assertRaises(FunctionCallException) as re:
            activities = service.search_activity("test", 'test')

        self.assertEqual(str(re.exception), "The type must be either 'date', 'time' or 'description")



    def test_add_person_to_activity(self):

        repo = ActivityRepository()
        undo_service = UndoService()
        service = ActivityService(repo, undo_service)
        service.add(1, [1, 2], date(2021, 11, 20), time(18, 0, 0), "Very Cool Activity")

        activity = service.get_activity_by_id(1)

        service.add_person_to_activity(activity, 3)

