import unittest

from src.domain.activity import Activity
from src.domain.domain_exceptions import ActivityException
from datetime import date, time


class TestActivity(unittest.TestCase):
    
    def test_activity(self):

        with self.assertRaises(TypeError) as te:
            ac = Activity()

        self.assertEqual(str(te.exception), "__init__() missing 5 required positional arguments: '_id', 'people_ids', '_date', '_time', and 'description'")

        activity = Activity(1, [1, 2], date(2021, 11, 18), time(18, 0, 0), "Very Cool Activity")

        self.assertEqual(activity.id, 1)
        self.assertEqual(activity.people, [1, 2])
        self.assertEqual(activity.date, date(2021, 11, 18))
        self.assertEqual(activity.time, time(18, 0, 0))
        self.assertEqual(activity.description, "Very Cool Activity")

        with self.assertRaises(ActivityException) as re:
            Activity(-1, [1, 2], date(2021, 11, 18), time(18, 0, 0), "Very Cool Activity")

        self.assertEqual(str(re.exception), "ID must be a positive integer")

        with self.assertRaises(ActivityException) as re:
            Activity("Test", [1, 2], date(2021, 11, 18), time(18, 0, 0), "Very Cool Activity")

        self.assertEqual(str(re.exception), "ID must be an integer")

        with self.assertRaises(ActivityException) as re:
            Activity(1, [1, 2], "2021-11-20", time(18, 0, 0), "Very Cool Activity")

        self.assertEqual(str(re.exception), "Type of `new_value` must be `datetime.date`")

        with self.assertRaises(ActivityException) as re:
            activity_2 = Activity(1, [1, 2], date(2021, 11, 18), "18:00", "Very Cool Activity")

        self.assertEqual(str(re.exception), "Type of `new_value` must be `datetime.time`")

        self.assertEqual(str(activity),  "ID: 1  |  Date: 2021-11-18  |  Time: 18:00  |  Description: Very Cool Activity\n   People in this activity: [1, 2]")
        self.assertEqual(repr(activity),  "ID: 1  |  Date: 2021-11-18  |  Time: 18:00  |  Description: Very Cool Activity\n   People in this activity: [1, 2]")


        activity.id = 2
        activity.people = [1]
        activity.date = date(2022, 11, 20)
        activity.time = time(18, 0, 0)
        activity.description = 'Test'

        self.assertEqual(activity.id, 2)
        self.assertEqual(activity.people, [1])
        self.assertEqual(activity.date, date(2022, 11, 20))
        self.assertEqual(activity.time, time(18, 0, 0))
        self.assertEqual(activity.description, 'Test')
