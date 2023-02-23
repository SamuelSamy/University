
from src.repository.repo_exceptions import RepositoryException


class PersonRepository:

    def __init__(self):
        """
        Initializes a repo object
        """
        self.__data = {}


    def add(self, person):
        """
        Adds `person` to the repository
        Args:
            person (Person): A person object

        Raises:
            RepositoryException: If there's already a person with the specified ID
        """
        if self.person_exists(person.id):
            raise RepositoryException("Duplicate Person ID")

        self.__data[person.id] = person


    def remove(self, person_id):
        """
        Removes the person object that has the specified ID from repo

        Args:
            person_id (int): Person's ID

        Raises:
            RepositoryException: If there's no person with the specified ID
        """
        if not self.person_exists(person_id):
            raise RepositoryException("No person found with the specified ID")

        del self.__data[person_id]


    def update(self, person_id, new_name, new_phone_number):
        """
        Updates the Person object with the specified id

        Args:
            person_id (int): Person's ID
            new_name (string): New Name
            new_phone_number (string): New Phone Number

        Raises:
            RepositoryException: If there's no person with the specified ID
        """
        if not self.person_exists(person_id):
            raise RepositoryException("No person found with the specified ID")

        person = self.__data[person_id]

        person.name = new_name or person.name
        person.phone_number = new_phone_number or person.phone_number


    def get_people(self):
        """
        Returns:
            list: A list of Person
        """
        people = []

        for person in self.__data.values():
            people.append(person)

        return people


    def get_people_ids(self):
        """
        Returns:
            list: A list of IDs (people)
        """
        ids = []

        for person in self.get_people():
            ids.append(person.id)

        return ids


    def person_exists(self, person_id):
        """
        Checks if `person_id` is already in the program
        Args:
            person_id (int): A Person's ID

        Returns:
            bool: True if a person with the specified id exists, False otherwise
        """
        return person_id in self.__data.keys()

    
    def get_person_by_id(self, person_id):
        """
        Args:
            person_id (int): A Person's ID

        Returns:
            Person: The person object with the specified ID
        """
        if person_id not in self.__data.keys():
            raise RepositoryException("No person with the specified ID found")

        return self.__data[person_id]


