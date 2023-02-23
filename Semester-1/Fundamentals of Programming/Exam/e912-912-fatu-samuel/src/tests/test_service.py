import unittest
from service.service import Service
from repository.repository import Repository
from service.exceptions import CustomException


class ServiceTests(unittest.TestCase):

    def test_verify_all(self):

        repository = Repository()
        service = Service(repository)

        with self.assertRaises(CustomException):
            service._verify_all(1000000, 10, 10)

        with self.assertRaises(CustomException):
            service._verify_all(-10000000000, 10, 10)

        with self.assertRaises(CustomException):
            service._verify_all(0, 100000000000000000, 10)

        with self.assertRaises(CustomException):
            service._verify_all(0, 0, 100000000000000000)

        service._verify_all(0, 0, 0)



    def test_manage_land(self):

        repository = Repository()
        service = Service(repository)

        service.manage_land(100)
        self.assertEqual(service.get_land(), 1100)

        service.manage_land(-100)
        self.assertEqual(service.get_land(), 1000)


    def test_manage_harvest(self):

        repository = Repository()
        service = Service(repository)
        service.manage_harvest()
        self.assertEqual(service.get_grain_stocks(), 5800)


    def test_manage_rats(self):

        repository = Repository()
        service = Service(repository)
        service.manage_rats()
        self.assertEqual(service.get_grain_stocks(), 2520)


    def test_manage_planint(self):
        repository = Repository()
        service = Service(repository)
        service.manage_planting(100)
        self.assertEqual(service.get_grain_stocks(), 300)


    def test_get_rats_ate(self):
        repository = Repository()
        service = Service(repository)
        self.assertEqual(service.get_rats_ate(), 200)


    def test_get_year(self):
        repository = Repository()
        service = Service(repository)
        self.assertEqual(service.get_year(), 1)


    def test_get_new_people(self):
        repository = Repository()
        service = Service(repository)
        self.assertEqual(service.get_new_people(), 0)



    def test_get_population(self):
        repository = Repository()
        service = Service(repository)
        self.assertEqual(service.get_population(), 100)


    def test_get_land(self):
        repository = Repository()
        service = Service(repository)
        self.assertEqual(service.get_land(), 1000)


    def test_increment_year(self):
        repository = Repository()
        service = Service(repository)
        service.increment_year()
        self.assertEqual(service.get_year(), 2)


    def test_manage_feeding(self):
        repository = Repository()
        service = Service(repository)
        service.manage_feeding(2000)
        self.assertEqual(service.get_grain_stocks(), 800)



