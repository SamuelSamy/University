import unittest

from src.repository.repository import Repository
from src.service.service import Service


class Tests(unittest.TestCase):


    def test_get_closest_taxi(self):

        repo = Repository()
        service = Service(repo)


        service.add_taxi(0, (0, 0))
        service.add_taxi(1, (100, 100))

        taxi = service.get_closest_taxi((1, 1))
        self.assertEqual(taxi.id, 0)

        """taxi = self.get_closest_taxi(start)
        self.increment_fare(taxi, start, stop)
        self.change_taxi_location(taxi, stop)"""


    def test_increment_fare(self):
        repo = Repository()
        service = Service(repo)

        taxi = service.add_taxi(0, (0, 0))

        service.increment_fare(taxi, (0, 0), (1, 1))

        self.assertEqual(taxi.total_fare, 2)


    def test_change_taxi_location(self):

        repo = Repository()
        service = Service(repo)

        taxi = service.add_taxi(0, (0, 0))

        service.change_taxi_location(taxi, (100, 100))

        self.assertEqual(taxi.location, (100, 100))
