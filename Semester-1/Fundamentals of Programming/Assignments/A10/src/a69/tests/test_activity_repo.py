import unittest

from src.a69.repository.repo_exceptions import RepositoryException
from src.a69.repository.activity_repo import ActivityRepository
from src.a69.domain.activity import Activity
from datetime import date, time


class TestActivityRepo(unittest.TestCase):

        
    def test_get_activity_by_id(self):

        repo = ActivityRepository()
        repo.add(Activity(1, [1, 2], date(2021, 11, 20), time(18, 0, 0), "Very Cool Activity"))

        activity = repo.get_activity_by_id(1)

        self.assertEqual(activity.id, 1)
        self.assertEqual(activity.description, "Very Cool Activity")


    def test_get_people_ids_in_activity(self):

        repo = ActivityRepository()
        repo.add(Activity(1, [1, 2], date(2021, 11, 20), time(18, 0, 0), "Very Cool Activity"))

        activity = repo.get_activity_by_id(1)
        people = repo.get_people_ids_in_activity(activity)

        self.assertEqual(people, [1, 2])


    def test_add(self):
        
        repo = ActivityRepository()

        repo.add(Activity(1, [1, 2], date(2021, 11, 20), time(18, 0, 0), "Very Cool Activity"))

        with self.assertRaises(RepositoryException) as re:
            repo.add(Activity(1, [1, 2], date(2021, 11, 20), time(18, 0, 0), "Very Cool Activity"))


        self.assertEqual(str(re.exception), "Duplicate Activity ID")
            
        with self.assertRaises(RepositoryException) as re:
            repo.add(Activity(2, [1, 2], date(2021, 11, 20), time(19, 0, 0), "Very Cool Activity"))

        self.assertEqual(str(re.exception), "Activities must not overlap!")


    def test_update(self):

        repo = ActivityRepository()
        repo.add(Activity(1, [1, 2], date(2021, 11, 20), time(18, 0, 0), "Very Cool Activity"))
    

        repo.update(1, [1], date(2022, 11, 20), time(20, 0, 0), "Test")

        activity = repo.get_activity_by_id(1)

        self.assertEqual(activity.people, [1])
        self.assertEqual(activity.date, date(2022, 11, 20))
        self.assertEqual(activity.time, time(20, 0, 0))
        self.assertEqual(activity.description, "Test")


        with self.assertRaises(RepositoryException) as re:
            repo.get_activity_by_id(10)

        self.assertEqual(str(re.exception), "No activity found with the specified ID")

        with self.assertRaises(RepositoryException) as re:
            repo.update(2, [1], date(2022, 11, 20), time(20, 0, 0), "Test")

        self.assertEqual(str(re.exception), "No activity found with the specified ID")

        repo.add(Activity(2, [1, 2], date(2021, 11, 20), time(5, 0, 0), "Very Cool Activity"))

        with self.assertRaises(RepositoryException) as re:
            repo.update(2, [1], date(2022, 11, 20), time(20, 0, 0), "Test")

        self.assertEqual(str(re.exception), "Can not update the activity. There's already an activity at the given time!")


        repo.update(1, [1, 2], None, None, None)



    def test_remove(self):

        repo = ActivityRepository()
        repo.add(Activity(1, [1, 2], date(2021, 11, 20), time(18, 0, 0), "Very Cool Activity"))

        with self.assertRaises(RepositoryException) as re:
            repo.remove(2)

        self.assertEqual(str(re.exception), "No activity found with the specified ID")

        repo.remove(1)

    def test_remove_person_from_activity(self):

        repo = ActivityRepository()
        repo.add(Activity(1, [1, 2], date(2021, 11, 20), time(18, 0, 0), "Very Cool Activity"))
        repo.add(Activity(2, [2], date(2021, 12, 20), time(18, 0, 0), "Very Cool Activity"))

        activity = repo.get_activity_by_id(1)


        repo.remove_person_from_activity(activity, 1)

        with self.assertRaises(RepositoryException) as re:
            repo.remove_person_from_activity(activity, 5)

        self.assertEqual(str(re.exception), "The specified person is not in this activity")

    def test_activity_overlaps(self):

        repo = ActivityRepository()
        overlaps = repo.activity_overlaps(date(2021, 10, 20), time(18, 0))
        self.assertEqual(overlaps, False)


    def test_add_person_to_activity(self):

        repo = ActivityRepository()
        repo.add(Activity(1, [1], date(2021, 11, 20), time(18, 0, 0), "Test"))

        activity = repo.get_activity_by_id(1)


        repo.add_person_to_activity(activity, 2)

        with self.assertRaises(RepositoryException) as re:
            repo.add_person_to_activity(activity, 1)

        self.assertEqual(str(re.exception), "This person is already in this activity")

    def test_get_activities(self):

        repo = ActivityRepository()
        repo.add(Activity(1, [1], date(2021, 11, 20), time(18, 0, 0), "Test"))

        activities = repo.get_activities()

        self.assertEqual(len(activities), 1)
