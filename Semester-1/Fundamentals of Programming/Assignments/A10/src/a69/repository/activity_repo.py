
from src.a69.repository.repo_exceptions import RepositoryException
from datetime import datetime
from src.a10.MyDataStructure import MyDataStructure


class ActivityRepository:

    def __init__(self):
        """
        Initializes a repository for activities
        """
        self._data = MyDataStructure()


    def add(self, activity):
        """
        Adds `activity` to the repository
        Args:
            activity (Activity): An activity object

        Raises:
            RepositoryException: If there's already an activity with the specified ID
        """
        for old_activity in self._data:
            if old_activity.id == activity.id:
                raise RepositoryException("Duplicate Activity ID")

        if self.activity_overlaps(activity.date, activity.time):
            raise RepositoryException("Activities must not overlap!")

        self._data.add(activity)


    def remove(self, activity_id):
        """
        Removes the activity object that has the specified ID from repo

        Args:
            activity_id (int): Activity's ID

        Raises:
            RepositoryException: If there's no activity with the specified ID
        """
        index = 0
        while index < len(self._data):

            if self._data[index].id == activity_id:
                del self._data[index]
                return

            index += 1

        raise RepositoryException("No activity found with the specified ID")



    def update(self, activity_id, new_people_ids, new_date, new_time, new_description):
        """
        Updates the Activity object with the specified id

        Args:
            activity_id (int): Activity's ID
            new_people_ids (list): A list with people IDs
            new_date (datetime.date): Activity's date
            new_time (datetime.time): Activity's time
            new_description (string): Activity's description

        Raises:
            RepositoryException: If there's no activity with the specified ID
            RepositoryException: If a user is already in another activity at the given time
        """

        activity = None
        for old_activity in self._data:
            if old_activity.id == activity_id:
                activity = self.get_activity_by_id(activity_id)
                break

        if activity is None:
            raise RepositoryException("No activity found with the specified ID")

        activity = self.get_activity_by_id(activity_id)

        if new_time is not None or new_date is not None:
            new_date = new_date or activity.date
            new_time = new_time or activity.time

            if self.activity_overlaps(new_date, new_time, activity_id):
                raise RepositoryException("Can not update the activity. There's already an activity at the given time!")

        activity.people = new_people_ids if new_people_ids is not None else activity.people
        activity.date = new_date if new_date is not None else activity.date
        activity.time = new_time if new_time is not None else activity.time
        activity.description = new_description if new_description is not None else activity.description

        return activity


    def get_activities(self):
        """
        Returns:
            MyDataStructure: A MyDataStructure of activities
        """
        return self._data


    def remove_person_from_activity(self, activity, person_id):
        """
        Removes `person_id` from this activity
        Args:
            activity (Activity): An Activity object
            person_id (int): Person's ID
        """

        if person_id not in activity.people:
            raise RepositoryException("The specified person is not in this activity")

        activity.people.remove(person_id)
        return activity


    def add_person_to_activity(self, activity, person_id):
        """
        Adds `person_id` to this activity

        Args:
            activity (Activity): An Activity object
            person_id (int): Person's ID
        """

        if person_id in activity.people:
            raise RepositoryException("This person is already in this activity")

        activity.people.append(person_id)
        return activity


    def get_activity_by_id(self, _id):
        """
        Args:
            _id (int): Activity's ID

        Returns:
            Activity: The Activity object

        Raises:
            RepositoryException: If there's no activity with the specified ID
        """

        for activity in self._data:
            if activity.id == _id:
                return activity

        raise RepositoryException("No activity found with the specified ID")


    def get_people_ids_in_activity(self, activity):
        """
        Returns a list with the IDs
        Args:
            activity (Activity): An Activity object

        Returns:
            list: A list of IDs
        """
        ids = []

        for person in activity.people:
            ids.append(person)

        return ids


    def activity_overlaps(self, _date, _time, ignore_activity_id=None):
        """
        Checks if a new activity overlaps with an existing one
        Args:
            _date (datetime.date): The new activity date
            _time (datetime.time): The new activity time
            ignore_activity_id (int): Activity to be ignored

        Returns:
            bool: True if the activity overlaps, False otherwise
        """

        activities = self.get_activities()

        max_activity_time = 2 * 60 * 60
        date_time = datetime(_date.year, _date.month, _date.day, _time.hour, _time.minute)

        for activity in activities:
            if activity.id != ignore_activity_id:

                activity_time = datetime(activity.date.year, activity.date.month, activity.date.day, activity.time.hour, activity.time.minute)
                diff = date_time - activity_time

                if diff.days == 0 and diff.seconds < max_activity_time:
                    return True

        return False


