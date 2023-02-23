from src.domain.domain_exceptions import ActivityException
import datetime


class Activity:

    def __init__(self, _id, people_ids, _date, _time, description):
        """
        Creates 

        Args:
            _id (int): Activity's ID
            people_ids (list): A list of IDs representing the people that participate in this activity
            _date (datetime.date): A datetime.date object representing activity's date
            _time (datetime.time): A datetime.time object representing activity's time
            description (string): Activity's description
        """
        self.id = _id
        self.date = _date
        self.time = _time
        self.description = description

        self.people = people_ids


    @property
    def id(self):
        """
        Returns:
            int: Activity's ID
        """
        return self.__id


    @id.setter
    def id(self, new_value):
        """
        Sets Activity's ID

        Args:
            new_value (int): New Activity's ID

        Raises:
            ActivityException: If the ID is not a positive integer
        """

        try:
            self.__id = int(new_value)
            assert self.__id >= 0
        except ValueError:
            raise ActivityException("ID must be an integer")
        except AssertionError:
            raise ActivityException("ID must be a positive integer")


    @property
    def date(self):
        """
        Returns:
            datetime.date: Activity's date
        """
        return self.__date


    @date.setter
    def date(self, new_value):
        """
        Sets Activity's date

        Args:
            new_value (datetime.date): New Activity's date

        Raises:
            ActivityException: If `new_value` is not of type `datetime.date`
        """

        if not isinstance(new_value, datetime.date):
            raise ActivityException("Type of `new_value` must be `datetime.date`")

        self.__date = new_value


    @property
    def time(self):
        """
        Returns:
            datetime.time: Activity's time
        """
        return self.__time


    @time.setter
    def time(self, new_value):
        """
        Sets Activity's time

        Args:
            new_value (datetime.time):  New Activity's time

        Raises:
            ActivityException: If `new_value` is not of type `datetime.time`
        """

        if not isinstance(new_value, datetime.time):
            raise ActivityException("Type of `new_value` must be `datetime.time`")

        self.__time = new_value


    @property
    def description(self):
        """
        Returns:
            string: Activity's description
        """
        return self.__description


    @description.setter
    def description(self, new_value):
        """
        Sets Activity's description
        Args:
            new_value (string): New Activity's description
        """
        self.__description = new_value


    @property
    def people(self):
        """
        Returns:
            list: A list of people that participate in this activity
        """
        return self.__people


    @people.setter
    def people(self, new_value):
        """
        Changes the list of people that participate in this activity to `new_value`
        Args:
            new_value (list): A new list of people that participate in this activity
        """
        self.__people = new_value
        

    def __str__(self):
        return f"ID: {self.id}  |  Date: {self.date}  |  Time: {str(self.time)[:-3]}  |  Description: {self.description}\n   People in this activity: {self.people}"

    def __repr__(self):
        return str(self)
