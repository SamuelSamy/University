from src.a69.services.serv_expections import FunctionCallException, UndoException
from src.a69.repository.repo_exceptions import RepositoryException
from src.a69.domain.domain_exceptions import PersonException, ActivityException
from src.a69.ui.ui_exceptions import UIException

import datetime


class UI:

    def __init__(self, activity_service, person_service, undo_service, generate = False):

        self.__activity_service = activity_service
        self.__person_service = person_service
        self.__undo_service = undo_service

        if generate:
            self.generate_items()


    def generate_items(self):
        self.__undo_service._record_flag = False
        self.__person_service.generate_people(20)
        self.__activity_service.generate_activities(20, self.__person_service.get_people_ids())
        self.__undo_service._record_flag = True


    def _print_menu(self):

        print("\n1. Manage people")
        print("2. Manage activities")
        print("3. Search")
        print("4. Statistics")
        print("5. Undo")
        print("6. Redo")
        print("7. Exit\n")


    def main(self):

        while True:

            self._print_menu()
            option = input("<< Enter an option: ").strip()

            try:

                if option == "1":
                    self._manage_people_ui()
                elif option == "2":
                    self._manage_activities_ui()
                elif option == "3":
                    self._handle_search()
                elif option == "4":
                    self._handle_statistics()
                elif option == "5":
                    self._handle_undo()
                elif option == "6":
                    self._handle_redo()
                elif option == "7":
                    return
                else:
                    print("• Invalid Option")

            except FunctionCallException as fce:
                print(f"• {fce}")

            except RepositoryException as re:
                print(f"• {re}")

            except UIException as uie:
                print(f"• {uie}")

            except ActivityException as ae:
                print(f"• {ae}")

            except PersonException as pe:
                print(f"• {pe}")

            except UndoException as ue:
                print(f"• {ue}")

            # except:
            #     print("• Unexpected error")


    def _manage_people_ui(self):

        return_to_main_menu = False

        while not return_to_main_menu:

            return_to_main_menu = False

            print("\n1. Add person")
            print("2. Remove person")
            print("3. Update person")
            print("4. List people")
            print("5. Return to main menu\n")

            suboption = input("<< Enter a suboption: ").strip()

            try:
                if suboption == "1":
                    self._add_person_ui()
                elif suboption == "2":
                    self._remove_person_ui()
                elif suboption == "3":
                    self._update_person_ui()
                elif suboption == "4":
                    self._list_people_ui()
                elif suboption == "5":
                    return_to_main_menu = True
                else:
                    print("• Invalid suboption. Please enter a new suboption!")

            except FunctionCallException as fce:
                print(f"• {fce}")

            except RepositoryException as re:
                print(f"• {re}")

            except ActivityException as ae:
                print(f"• {ae}")

            except PersonException as pe:
                print(f"• {pe}")

            except UIException as uie:
                print(f"• {uie}")


    def _add_person_ui(self):

        try:
            _id = int(input("<< Enter the ID: ").strip())
        except:
            raise UIException("ID must be an integer")

        name = input("<< Enter a name: ").strip()
        phone_number = input("<< Enter a phone number: ").strip()

        self.__person_service.add(_id, name, phone_number)

        print("• The person was added successfully!")


    def _remove_person_ui(self):

        try:
            _id = int(input("<< Enter the ID: ").strip())
        except:
            raise UIException("• The ID must be an integer!")

        self.__person_service.remove(_id)

        print("• Person removed successfully!")


    def _update_person_ui(self):

        try:
            _id = int(input("<< Enter the ID: ").strip())
        except:
            raise UIException("• ID must be an integer")

        new_name = None
        new_phone_number = None

        answer = ""
        while answer not in ['Y', 'n']:
            answer = input("<< Would you like to change this person's name? (Y/n) ").strip()

            if answer not in ['Y', 'n']:
                print("• Please enter a valid option (Y/n)")

        if answer == 'Y':
            new_name = input("<< Enter a new name: ")

        answer = ""
        while answer not in ['Y', 'n']:
            answer = input("<< Would you like to change this person's phone number? (Y/n) ").strip()

            if answer not in ['Y', 'n']:
                print("• Please enter a valid option (Y/n)")

        if answer == 'Y':
            new_phone_number = input("<< Enter a new phone number: ")

        self.__person_service.update(_id, new_name, new_phone_number)

        print("• Person updated successfully!")


    def _list_people_ui(self):

        people = self.__person_service.get_sorted_people(lambda person0, person1: person0.id < person1.id)

        if len(people) == 0:
            print("• No people found!")
        else:
            for person in people:
                print(person)


    # Activities


    def _manage_activities_ui(self):

        return_to_main_menu = False

        while not return_to_main_menu:

            return_to_main_menu = False

            print("\n1. Add activity")
            print("2. Remove activity")
            print("3. Update activity")
            print("4. List activities")
            print("5. Return to main menu\n")

            suboption = input("<< Enter a suboption: ").strip()

            try:
                if suboption == "1":
                    self._add_activity_ui()
                elif suboption == "2":
                    self._remove_activity_ui()
                elif suboption == "3":
                    self._update_activity_ui()
                elif suboption == "4":
                    self._list_activities_ui()
                elif suboption == "5":
                    return_to_main_menu = True
                else:
                    print("• Invalid suboption!")

            except FunctionCallException as fce:
                print(f"• {fce}")

            except RepositoryException as re:
                print(f"• {re}")

            except ActivityException as ae:
                print(f"• {ae}")

            except PersonException as pe:
                print(f"• {pe}")

            except UIException as uie:
                print(f"• {uie}")


    def _add_activity_ui(self):

        _id = input("<< Enter the ID: ").strip()

        try:
            _id = int(_id)
        except:
            raise ActivityException("The ID must be an integer!")


        date = self._read_date()
        time = self._read_time()
        description = input("<< Enter a description: ")

        people = self._read_people()

        if people is None:
            return

        self.__activity_service.add(_id, people, date, time, description)

        print("• The activity was added successfully!")


    def _remove_activity_ui(self):

        try:
            _id = int(input("<< Enter the ID: ").strip())
        except:
            raise UIException("• ID must be an integer")

        self.__activity_service.remove(activity_id = _id)

        print("• Activity removed successfully!")


    def _update_activity_ui(self):

        try:
            _id = int(input("Enter the ID: ").strip())
        except:
            raise UIException("• ID must be an integer")


        activity = self.__activity_service.get_activity_by_id(_id)


        new_people = None
        new_date = None
        new_time = None
        new_description = None


        answer = ""
        while answer not in ['Y', 'n']:
            answer = input("<< Would you like to change this activity's date? (Y/n) ").strip()

            if answer not in ['Y', 'n']:
                print("• Please enter a valid option (Y/n)")

        if answer == 'Y':
            new_date = self._read_date()


        answer = ""
        while answer not in ['Y', 'n']:
            answer = input("<< Would you like to change this activity's time? (Y/n) ").strip()

            if answer not in ['Y', 'n']:
                print("• Please enter a valid option (Y/n)")


        if answer == 'Y':
            new_time = self._read_time()


        answer = ""
        while answer not in ['Y', 'n']:
            answer = input("<< Would you like to change this activity's description? (Y/n) ").strip()

            if answer not in ['Y', 'n']:
                print("• Please enter a valid option (Y/n)")

        if answer == 'Y':
            new_description = input("<< Enter a description: ")


        answer = ""
        while answer not in ['Y', 'n']:
            answer = input("<< Would you like to change this activity's people? (Y/n) ").strip()

            if answer not in ['Y', 'n']:
                print("• Please enter a valid option (Y/n)")

        if answer == 'Y':
            new_people = self._read_people()

            if new_people is None:
                return

        self.__activity_service.update(_id, new_people, new_date, new_time, new_description)

        print("• Activity updated successfully!")


    def _list_activities_ui(self):

        activities = self.__activity_service.get_sorted_activities(lambda activity0, activity1: activity0.id < activity1.id)

        if len(activities) == 0:
            print("• No activities found!")
        else:
            for activity in activities:
                print(activity)

    # Extra

    def _read_date(self):

        date_is_valid = False
        _date = None
        print("• Specify a date:")

        while not date_is_valid:

            day = input("<< Enter a day: ").strip()
            month = input("<< Enter a month: ").strip()
            year = input("<< Enter a year: ").strip()

            try:
                day = int(day)
                month = int(month)
                year = int(year)

                _date = datetime.date(year, month, day)
                date_is_valid = True
            except:
                print("• Please enter a valid date")
                continue

        return _date


    def _read_people(self):

        people_number = input("<< Enter the number of people: ").strip()

        try:
            people_number = int(people_number)
            assert people_number >= 0
        except:
            raise UIException("Invalid input. Expected a positive number")


        people_list = []
        i = 0
        while i < people_number:

            person_id = input(f"<<  ({i+1}/{people_number}) Enter a person ID (-1 to return to the main menu): ").strip()

            try:
                person_id = int(person_id)
            except:
                print("• ID must be a number")
                continue

            if person_id == -1:
                print("• Returning to the main menu...")
                return None

            if not self.__person_service.person_exists(person_id):
                print("• No person with specified ID found")
                continue

            people_list.append(person_id)
            i += 1

        return people_list


    def _read_time(self):

        time_is_valid = False
        _time = None

        print("• Specify a time:")

        while not time_is_valid:

            hour = input("<< Enter an hour: ").strip()
            minute = input("<< Enter minutes: ").strip()

            try:
                hour = int(hour)
                minute = int(minute)

                _time = datetime.time(hour, minute, 0)
                time_is_valid = True
            except:
                print("• Please enter a valid time")
                continue

        return _time

    # Search

    def _handle_search(self):

        return_to_main_menu = False

        while not return_to_main_menu:

            return_to_main_menu = False

            print("\n1. Search people by name")
            print("2. Search people by phone number")
            print("3. Search activities by date")
            print("4. Search activities by time")
            print("5. Search activities by description")
            print("6. Return to main menu\n")

            suboption = input("<< Enter a suboption: ").strip()

            try:
                if suboption == "1":
                    self._search_people('name')
                elif suboption == "2":
                    self._search_people('number')
                elif suboption == "3":
                    self._search_activities_by_date()
                elif suboption == "4":
                    self._search_activities_by_time()
                elif suboption == "5":
                    self._search_activities_by_description()
                elif suboption == "6":
                    return_to_main_menu = True
                else:
                    print("• Invalid suboption. Please enter a new suboption!")

            except FunctionCallException as fce:
                print(f"• {fce}")

            except RepositoryException as re:
                print(f"• {re}")

            except ActivityException as ae:
                print(f"• {ae}")

            except PersonException as pe:
                print(f"• {pe}")

            except UIException as uie:
                print(f"• {uie}")


    def _search_people(self, option):
        search_word = input("<< Enter text to search: ")
        people = self.__person_service.search_people(search_word, option)
        self._print_search_list(people)


    def _search_activities_by_date(self):
        date = input("<< If you want to search for a specific date follow this format: `yyyy-mm-dd`\n<< Enter text to search: ")
        activities = self.__activity_service.search_activity(date, 'date')
        self._print_search_list(activities)

    def _search_activities_by_time(self):
        time = input("<< If you want to search for a specific time follow this format: `hh:mm`\n<< Enter text to search: ")
        activities = self.__activity_service.search_activity(time, 'time')
        self._print_search_list(activities)


    def _search_activities_by_description(self):
        description = input("<< Enter text to search: ")
        activities = self.__activity_service.search_activity(description, 'description')
        self._print_search_list(activities)


    def _print_search_list(self, _list):

        if len(_list) == 0:
            print("• No matches found!")
        else:
            for _object in _list:
                print(_object)

    # Statistics

    def _handle_statistics(self):

        return_to_main_menu = False

        while not return_to_main_menu:

            return_to_main_menu = False

            print("\n1. Activities for a given date")
            print("2. Busiest days")
            print("3. Activities with a given person")
            print("4. Return to main menu\n")

            suboption = input("<< Enter a suboption: ").strip()

            try:
                if suboption == "1":
                    self._activities_in_a_given_date_ui()
                elif suboption == "2":
                    self._busiest_days_ui()
                elif suboption == "3":
                    self._activities_with_given_person_ui()
                elif suboption == "4":
                    return_to_main_menu = True
                else:
                    print("• Invalid suboption. Please enter a new suboption!")

            except FunctionCallException as fce:
                print(f"• {fce}")

            except RepositoryException as re:
                print(f"• {re}")

            except ActivityException as ae:
                print(f"• {ae}")

            except PersonException as pe:
                print(f"• {pe}")

            except UIException as uie:
                print(f"• {uie}")


    def _activities_in_a_given_date_ui(self):
        date = self._read_date()
        activities = self.__activity_service.get_activities_by_date(date)
        self._print_activities_statistics(activities)


    def _busiest_days_ui(self):
        dates = self.__activity_service.get_busiest_days()

        for date in dates:
            if date[1] == 1:
                print(f"Date: {date[0]} has {date[1]} activity")
            else:
                print(f"Date: {date[0]} has {date[1]} activities")


    def _activities_with_given_person_ui(self):
        try:
            _id = int(input("<< Enter person ID: ").strip())
        except:
            raise UIException("ID must be an integer")

        activities = self.__activity_service.get_activities_with_person(_id)
        self._print_activities_statistics(activities)


    def _print_activities_statistics(self, _list):

        if len(_list) == 0:
            print("• No activities found")
        else:
            for _item in _list:
                print(_item)


    # Undo / Redo

    def _handle_undo(self):
        self.__undo_service.undo()
        print("• Successfully undid")


    def _handle_redo(self):
        self.__undo_service.redo()
        print("• Successfully redid")
