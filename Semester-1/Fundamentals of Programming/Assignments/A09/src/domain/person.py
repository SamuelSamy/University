from src.domain.domain_exceptions import PersonException

import re


class Person:

    def __init__(self, _id, name, phone_number):

        self.id = _id
        self.name = name
        self.phone_number = phone_number


    @property
    def id(self):
        """
        Returns:
            int: Person's ID
        """
        return self.__id


    @id.setter
    def id(self, new_value):
        """
        Sets Person's ID
        Args:
            new_value (int): New ID

        Raises:
            PersonException: If the new ID is not a positive integer
        """
        try:
            new_value = int(new_value)
            assert new_value >= 0
        except ValueError:
            raise PersonException("Person's ID must be an integer")
        except AssertionError:
            raise PersonException("Person's ID must be a positive integer")

        self.__id = new_value


    @property
    def name(self):
        """
        Returns:
            string: Person's Name
        """
        return self.__name


    @name.setter
    def name(self, new_value):
        """
        Sets Person's Name
        Args:
            new_value (string): New Person's Name

        Raises:
            PersonException: If `new_value` is None or empty
        """
        if new_value is None or new_value.strip() == "":
            raise PersonException("Person's Name must be non-empty")

        self.__name = new_value.strip()


    @property
    def phone_number(self):
        """
        Returns:
            string: Person's phone number
        """
        return self.__phone_number


    @phone_number.setter
    def phone_number(self, new_value):
        """
        Sets Person's phone number

        Args:
            new_value (string): A phone number

        Raises:
            PersonException: If the number is invalid
        """
        correct_ro_number = re.match("^07\d{8}$", new_value)

        if not correct_ro_number:
            raise PersonException("Invalid Phone Number")

        self.__phone_number = new_value


    def __str__(self):
        return f"ID: {self.id}  |  Name: {self.name}  |  Phone number: {self.phone_number}"


    def __repr__(self):
        return str(self)
