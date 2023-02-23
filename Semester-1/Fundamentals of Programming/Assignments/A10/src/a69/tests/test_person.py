import unittest

from src.a69.domain.person import Person
from src.a69.domain.domain_exceptions import PersonException


class TestPerson(unittest.TestCase):

    def test_person(self):

        person = Person(1, "Sam", "0701234567")
        
        self.assertEqual(person.id, 1)
        self.assertEqual(person.name, "Sam")
        self.assertEqual(person.phone_number, "0701234567")

        with self.assertRaises(PersonException) as pe:
            person = Person("Test", "Dan", "0712345678")

        self.assertEqual(str(pe.exception), "Person's ID must be an integer")

        with self.assertRaises(PersonException) as pe:
            person = Person(-1, "Dan", "0712345678")

        self.assertEqual(str(pe.exception), "Person's ID must be a positive integer")

        with self.assertRaises(PersonException) as pe:
            person = Person(2, "", "0712345678")

        self.assertEqual(str(pe.exception), "Person's Name must be non-empty")

        with self.assertRaises(PersonException) as pe:
            person = Person(2, "Dan", "Test")

        self.assertEqual(str(pe.exception), "Invalid Phone Number")

        self.assertEqual(str(person), "ID: 1  |  Name: Sam  |  Phone number: 0701234567")
        self.assertEqual(repr(person), "ID: 1  |  Name: Sam  |  Phone number: 0701234567")


        person.id = 5
        person.name = "Test"
        person.phone_number = "0712345678"

        self.assertEqual(person.id, 5)
        self.assertEqual(person.name, "Test")
        self.assertEqual(person.phone_number, "0712345678")
