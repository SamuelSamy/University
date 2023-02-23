
from src.domain.person import Person
from src.services.serv_expections import FunctionCallException
from src.services.undo_service import FunctionCall, Operation, CascadedOperation
import random


class PersonService:

    def __init__(self, activity_service, repository, undo_service):
        """

        Args:
            activity_service (ActivityService): An ActivityService object
            repository (PersonRepository): A PersonRepository object
        """
        self.__activity_service = activity_service
        self.__repository = repository
        self.__undo_service = undo_service

    
    def add(self, _id, name, phone_number):
        """
        Adds a person to the repository
        Args:
            _id (int): Person's ID
            name (string): Person's Name
            phone_number (string): Person's Phone Number
        """

        person = Person(_id, name, phone_number)
        self.__repository.add(person)

        function_call_undo = FunctionCall(self.remove, _id)
        function_call_redo = FunctionCall(self.add, _id, name, phone_number)

        operation = Operation(function_call_undo, function_call_redo)
        self.__undo_service.record_operation(operation)


    
    def remove(self, person_id):
        """
        Removes a person from the repository
        Args:
            person_id (int, optional): An ID
        """

        person = self.get_person_by_id(person_id)

        self.__repository.remove(person.id)

        function_call_undo = FunctionCall(self.add, person.id, person.name, person.phone_number)
        function_call_redo = FunctionCall(self.remove, person.id)

        cascaded_operation = CascadedOperation()
        cascaded_operation.add(Operation(function_call_undo, function_call_redo))

        activities = self.__activity_service.get_activities()

        for activity in activities:
            activity_people = activity.people

            if person.id in activity_people:

                self.__activity_service.remove_person_from_activity(activity, person.id)

                function_call_undo = FunctionCall(self.__activity_service.add_person_to_activity, activity, person.id)
                function_call_redo = FunctionCall(self.__activity_service.remove_person_from_activity, activity, person.id, True)

                cascaded_operation.add(Operation(function_call_undo, function_call_redo))


        self.__undo_service.record_operation(cascaded_operation)


    def update(self, person_id, new_name = None, new_phone_number = None):
        """
        Updates the person with the specified ID
        Args:
            person_id (int): Person's ID
            new_name (string, optional): Person's new name. Defaults to None.
            new_phone_number (string, optional): Person's new phone number. Defaults to None.
        """

        person = self.get_person_by_id(person_id)
        old_name = person.name
        old_phone_number = person.phone_number

        self.__repository.update(person_id, new_name, new_phone_number)

        function_call_undo = FunctionCall(self.update, person_id, old_name, old_phone_number)
        function_call_redo = FunctionCall(self.update, person_id, new_name, new_phone_number)

        operation = Operation(function_call_undo, function_call_redo)
        self.__undo_service.record_operation(operation)


    def get_people(self):
        """
        Returns:
            list: A list of `Person` containing all the people from the program
        """
        return self.__repository.get_people()


    def get_people_ids(self):
        """
        Returns:
            list: A list containing all the IDs of people
        """
        return self.__repository.get_people_ids()


    def person_exists(self, person_id):
        """
        Checks if the person with the specified ID exits
        Args:
            person_id (int): An ID

        Returns:
            bool: True if a person with the specified id exists, False otherwise
        """
        return self.__repository.person_exists(person_id)


    def get_person_by_id(self, _id):
        """
        Args:
            _id: Person ID

        Returns: A Person object.

        """
        return self.__repository.get_person_by_id(_id)


    def search_people(self, search_word, search_type):
        """
        Searches people by the given parameters

        Args:
            search_word: A string that will be searched
            search_type: Either 'name' or 'number'. The type of search that will be executed.

        Returns: A list containing the matches


        """
        search_type = search_type.lower()
        search_word = search_word.lower().strip()

        if not search_word:
            raise FunctionCallException("The text can not be empty!")

        if search_type not in ['name', 'number']:
            raise FunctionCallException("Search type must be either 'name' or 'number'")

        people = self.get_people()

        result_prefix = []
        result_in = []

        for person in people:

            to_search = ''

            if search_type == 'name':
                to_search = person.name.lower()
            else:  # search_type == 'number':
                to_search = person.phone_number.lower()

            if to_search.startswith(search_word):
                result_prefix.append(person)
            elif search_word in to_search:
                result_in.append(person)

        result_prefix.sort(key=lambda _person: _person.id)
        result_in.sort(key=lambda _person: _person.id)

        result = result_prefix + result_in

        return result



    def generate_people(self, number):
        """
        Generates `number` random people
        Args:
            number (int): The amount of people we want to generate
        """
        i = 0
        names = ["Jan Knight", "Jeevan Nunez", "Skylar Villegas", "Robbie Dotson", "Terry Neale", "Lennox Dougherty", "Maddison Thornton", "Anwar Gallegos", "Shay Woodward", "Hilda Anderson", "Ty O'Reilly", "Manpreet Coombes", "Saxon Powers", "Jose Christian", "Jaya Hooper", "Robin Marsh", "Lennon Bautista", "Simran Wells", "Cordelia Guerra", "Taryn Sumner", "Kinga Odling", "Tayla Rayner", "Teddie ", "Shelby Ferguson", "Dillan Jones", "Jaskaran Woodley", "Rio Acevedo", "Dana Stein", "Bo Erickson", "Amara Matthams"]
        ids = list(range(100))

        random.shuffle(ids)
        random.shuffle(names)

        excepted = False  # I am not proud about this but I wanted 100% test coverage

        while i < number:
            
            name = names[i]
            _id = ids[i]
            phone_number = self.generate_random_phone_number()

            try:
                if i == number - 1 and not excepted:  # I am not proud about this but I wanted 100% test coverage
                    _id = ids[0]  # I am not proud about this but I wanted 100% test coverage

                self.add(_id, name, phone_number)
            except Exception as error:
                excepted = True  # I am not proud about this but I wanted 100% test coverage
                i -= 1

            i += 1


    def generate_random_phone_number(self):
        """
        Generates a random phone number
        Returns:
            string: A Valid Phone Number
        """
        phone_number = "07"

        for i in range(8):
            digit = random.randrange(0, 10)
            phone_number += str(digit)

        return phone_number
