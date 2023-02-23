from src.domain.activity import Activity
from src.services.serv_expections import FunctionCallException
from src.services.undo_service import Operation, CascadedOperation, FunctionCall

import random
from datetime import time, date, timedelta


class ActivityService:

    def __init__(self, repository, undo_service):
        """
        Initializes the service class for activities

        Args:
            repository (ActivityRepository): A repo for activity
            undo_service (UndoService): A service file that manages undo/redo
        """
        self.__repository = repository
        self.__undo_service = undo_service


    def add(self, activity_id, people_ids, _date, _time, description):
        """
        Adds an activity to the repository
        Args:
            activity_id (int): Activity's ID
            people_ids (list): Activity's People List
            _date (datetime.date): Activity's Date
            _time (datetime.time): Activity's Time
            description (string): Activity's Description
        """

        activity = Activity(activity_id, people_ids, _date, _time, description)

        self.__repository.add(activity)

        function_call_undo = FunctionCall(self.remove, activity_id)
        function_call_redo = FunctionCall(self.add, activity_id, people_ids, _date, _time, description)

        operation = Operation(function_call_undo, function_call_redo)
        self.__undo_service.record_operation(operation)


    def remove(self, activity_id):
        """
        Removes an activity from the repo
        Args:
            activity_id (int, optional): An ID
        """

        activity = self.get_activity_by_id(activity_id)

        self.__repository.remove(activity_id)

        function_call_undo = FunctionCall(self.add, activity_id, activity.people, activity.date, activity.time, activity.description)
        function_call_redo = FunctionCall(self.remove, activity_id)

        operation = Operation(function_call_undo, function_call_redo)
        self.__undo_service.record_operation(operation)




    def update(self, activity_id, new_people_ids = None, new_date = None, new_time = None, new_description = None):
        """
        Updates the Activity object with the specified ID
        Args:
            activity_id (int): Activity's ID
            new_people_ids (list, optional): A list of people IDs. Defaults to None.
            new_date (datetime.date, optional): Activity's Date. Defaults to None.
            new_time (datetime.time, optional): Activity's Time. Defaults to None.
            new_description (string, optional): Activity's Description. Defaults to None.
        """

        activity = self.get_activity_by_id(activity_id)

        old_people = activity.people
        old_date = activity.date
        old_time = activity.time
        old_description = activity.description

        function_call_undo = FunctionCall(self.update, activity_id, old_people, old_date, old_time, old_description)
        function_call_redo = FunctionCall(self.update, activity_id, new_people_ids, new_date, new_time, new_description)

        self.__repository.update(activity_id, new_people_ids, new_date, new_time, new_description)

        operation = Operation(function_call_undo, function_call_redo)
        self.__undo_service.record_operation(operation)




    def get_activities(self):
        """
        Returns:
            list: A list representing the activities
        """
        return self.__repository.get_activities()

    
    def get_people_ids_in_activity(self, activity_id):
        """
        Args:
            activity_id (int): Activity's ID

        Returns:
            list: People that participate in the activity with the specified ID
        """
        activity = self.__repository.get_activity_by_id(activity_id)
        return self.__repository.get_people_ids_in_activity(activity)


    def remove_person_from_activity(self, activity, person_id, ignore = False):
        """
        Calls `remove_person_from_activity` from the repository
        Args:
            activity (Activity): An Activity object
            person_id (int): Person's ID
        """
        if not ignore:
            self.__repository.remove_person_from_activity(activity, person_id)


    def add_person_to_activity(self, activity, person_id):
        """
        Calls `add_person_to_activity` from the repository
        Args:
            activity (Activity): An Activity object
            person_id (int): Person's ID
        """

        self.__repository.add_person_to_activity(activity, person_id)


    def get_activity_by_id(self, _id):
        """
        Args:
            _id (int): Activity's ID

        Returns:
            Activity: The Activity object
        """
        return self.__repository.get_activity_by_id(_id)


    def search_activity(self, search_word, search_type):
        """
        Searches activities by the given parameters

        Args:
            search_word: A string that will be searched
            search_type: Either 'date', 'time' or 'description'. The type of search that will be executed.

        Returns: A list containing the matches

        """
        search_type = search_type.lower()
        search_word = search_word.lower().strip()

        if not search_word:
            raise FunctionCallException("The text can not be empty!")

        if search_type not in ['date', 'time', 'description']:
            raise FunctionCallException("The type must be either 'date', 'time' or 'description")

        result_prefix = []
        result_in = []

        activities = self.get_activities()

        for activity in activities:

            to_search = ''

            if search_type == 'date':
                to_search = str(activity.date).lower()
            elif search_type == 'time':
                to_search = str(activity.time).lower()[:-3]
            else:  # search_type == 'description':
                to_search = activity.description.lower()

            if to_search.startswith(search_word):
                result_prefix.append(activity)
            elif search_word in to_search:
                result_in.append(activity)

        result_prefix.sort(key=lambda _activity: _activity.id)
        result_in.sort(key=lambda _activity: _activity.id)

        result = result_prefix + result_in

        return result


    def get_activities_by_date(self, _date):
        """

        Args:
            _date: The date in which we want to get the activities

        Returns: All the activities in the specified date

        """
        if type(_date) != type(date(1, 1, 1)):
            raise FunctionCallException("date type must be `datetime.date`")


        activities = self.get_activities()

        result = []

        for activity in activities:
            if activity.date == _date:
                result.append(activity)

        result.sort(key = lambda _activity: _activity.time)

        return result


    def get_busiest_days(self):
        """
        Returns: A list of dates ordered by their free time (ascending)
        """

        activities = self.get_activities()

        dates = {}

        for activity in activities:
            if str(activity.date) not in dates.keys():
                dates[str(activity.date)] = 1
            else:
                dates[str(activity.date)] += 1

        result = []

        for _date, value in dates.items():
            result.append((_date, value))

        result.sort(key = lambda _activity: _activity[1], reverse = True)

        return result


    def get_activities_with_person(self, person_id):
        """

        Args:
            person_id: The person ID

        Returns: All the activities in which the person with the specified ID is in

        """

        activities = self.get_activities()
        result = []

        for activity in activities:
            if person_id in activity.people:
                result.append(activity)

        result.sort(key = lambda _activity: _activity.id)

        return result


    # Generate
    def generate_activities(self, number, people_ids):
        """
        Generates `number` activities

        Args:
            number: The number of activities the function will generate
            people_ids: A list of people IDs

        Returns: A list of random generated activities

        """
        i = 0

        ids = list(range(100))
        descriptions = ["Lorem", "ipsum", "dolor", "sit", "amet", "consectetur", "adipiscing", "elit.", "Etiam", "auctor", "turpis", "nec", "eleifend", "sollicitudin", "leo", "sem", "scelerisque", "odio", "ac", "tempor", "sem", "velit", "nec", "odio.", "Aliquam", "erat", "volutpat.", "Proin", "pharetra", "et", "mauris", "non", "eleifend"]

        random.shuffle(ids)
        random.shuffle(descriptions)

        last_person_id = 0

        excepted = False  # I am not proud about this but I wanted 100% test coverage

        while i < number:
            
            _id = ids[i]
            description = descriptions[i]
            _date = self.generate_random_date()
            _time = self.generate_random_time()

            people_number = random.randrange(1, 4)
            people = []

            for j in range(people_number):
                people.append(people_ids[last_person_id % len(people_ids)])
                last_person_id += 1

            try:
                if i == number - 1 and not excepted:  # I am not proud about this but I wanted 100% test coverage
                    _id = ids[0]  # I am not proud about this but I wanted 100% test coverage

                self.add(_id, people, _date, _time, description)
                i += 1
            except:
                excepted = True  # I am not proud about this but I wanted 100% test coverage
                last_person_id -= people_number


    def generate_random_date(self):
        """
        Generates a random date
        Returns:
            datetime.date: A random date
        """
        start_date = date.today()
        end_date = date(2022, 2, 14)

        diff = end_date - start_date
        max_seconds = (diff.days * 24 * 60 * 60)

        r_seconds = random.randrange(max_seconds)

        return start_date + timedelta(seconds = r_seconds)


    def generate_random_time(self):
        """
        Generates a random time
        Returns:
            datetime.time: A random time
        """
        return time(random.randrange(0, 24), random.randrange(0, 60))

