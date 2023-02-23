
from src.a69.repository.repo_exceptions import RepositoryException
from src.a10.MyDataStructure import MyDataStructure

class PersonRepository:

    def __init__(self):
        """
        Initializes a repo object
        """
        self._data = MyDataStructure()


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

        self._data.add(person)


    def remove(self, person_id):
        """
        Removes the person object that has the specified ID from repo

        Args:
            person_id (int): Person's ID

        Raises:
            RepositoryException: If there's no person with the specified ID
        """
        index = 0
        while index < len(self._data):

            if self._data[index].id == person_id:
                del self._data[index]
                return

            index += 1

        raise RepositoryException("No person found with the specified ID")



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

        person = None
        for old_activity in self._data:
            if old_activity.id == person_id:
                person = self.get_person_by_id(person_id)
                break

        if person is None:
            raise RepositoryException("No person found with the specified ID")

        person.name = new_name or person.name
        person.phone_number = new_phone_number or person.phone_number

        return person


    def get_people(self):
        """
        Returns:
            list: A list of People
        """
        return self._data



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
        for person in self._data:
            if person.id == person_id:
                return True

        return False


    
    def get_person_by_id(self, person_id):
        """
        Args:
            person_id (int): A Person's ID

        Returns:
            Person: The person object with the specified ID
        """

        for person in self._data:
            if person.id == person_id:
                return person

        raise RepositoryException("No person with the specified ID found")



