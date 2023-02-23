from tkinter import *
from tkinter import messagebox
import tkinter.font as font

import datetime

from src.repository.person_repo import RepositoryException
from src.ui.ui_exceptions import UIException
from src.services.undo_service import UndoException


class GUI:

    def __init__(self, activity_service, person_service, undo_service, generate = False):

        self.__activity_service = activity_service
        self.__person_service = person_service
        self.__undo_service = undo_service

        self.main_window = None
        self.tk = Tk()

        self.tk.option_add('*Dialog.msg.font', 'Helvetica 24')
        self.tk.option_add('*Dialog.msg.width', 40)
        self.tk.option_add("*Dialog.msg.wrapLength", "10i")

        if generate:
            self.generate_items()


    def generate_items(self):
        self.__undo_service._record_flag = False
        self.__person_service.generate_people(20)
        self.__activity_service.generate_activities(20, self.__person_service.get_people_ids())
        self.__undo_service._record_flag = True


    def activate_main_window(self):

        self.tk.title("FP Assignment")

        main_window = Canvas(self.tk, height = 400, width = 300)
        main_window.pack()
        self.main_window = main_window

        self.people_button = Button(main_window, text = "Manage People", command = self.people_button_pressed, height = 2, width = 20, font = font.Font(family = 'Time New Roman', size = 20))
        self.people_button.pack()
        
        self.activities_button = Button(main_window, text = "Manage Activities", command = self.activities_button_pressed, height = 2, width = 20, font = font.Font(family = 'Time New Roman', size = 20))
        self.activities_button.pack()

        self.activities_button = Button(main_window, text = "Search", command = self.search_button_pressed, height = 2, width = 20, font = font.Font(family = 'Time New Roman', size = 20))
        self.activities_button.pack()

        self.activities_button = Button(main_window, text = "Statistics", command = self.statistics_button_pressed, height = 2, width = 20, font = font.Font(family = 'Time New Roman', size = 20))
        self.activities_button.pack()

        self.activities_button = Button(main_window, text = "Undo", command = self.undo_button_pressed, height = 2, width = 20, font = font.Font(family = 'Time New Roman', size = 20))
        self.activities_button.pack()

        self.activities_button = Button(main_window, text = "Redo", command = self.redo_button_pressed, height = 2, width = 20, font = font.Font(family = 'Time New Roman', size = 20))
        self.activities_button.pack()

        self.main_window.mainloop() 
    

    def people_button_pressed(self):
        
        people_window = Toplevel(self.main_window)

        button = Button(people_window, text = "Add", command = self.add_person_gui, height = 2, width = 10, font = font.Font(family = 'Time New Roman',size = 14))
        button.grid(column = 0, row = 0)
        button = Button(people_window, text = "Remove",  command = self.remove_person_gui, height = 2, width = 10, font = font.Font(family = 'Time New Roman',size = 14))
        button.grid(column=0, row=1)
        button = Button(people_window, text = "Update",  command = self.update_person_gui, height = 2, width = 10, font = font.Font(family = 'Time New Roman',size = 14))
        button.grid(column=0, row=2)
        button = Button(people_window, text = "List",  command = self.lsit_people_gui, height = 2, width = 10, font = font.Font(family = 'Time New Roman',size = 14))
        button.grid(column=0, row=3)

        Label(people_window, text="ID:", font = font.Font(family = 'Time New Roman',size = 12)).grid(column = 1, row = 0)
        self.id_entry = Entry(people_window, {}, font = font.Font(family = 'Time New Roman',size = 12))
        self.id_entry.grid(column = 2, row = 0)

        Label(people_window, text="Name:", font = font.Font(family = 'Time New Roman',size = 12)).grid(column = 1, row = 1)
        self.name_entry = Entry(people_window, {}, font = font.Font(family = 'Time New Roman',size = 12))
        self.name_entry.grid(column = 2, row = 1)

        Label(people_window, text="Phone Number:", font = font.Font(family = 'Time New Roman',size = 12)).grid(column = 1, row = 2)
        self.phone_number_entry = Entry(people_window, {}, font = font.Font(family = 'Time New Roman',size = 12))
        self.phone_number_entry.grid(column = 2, row = 2)


    def add_person_gui(self):

        try:
            try:
                _id = int(self.id_entry.get().strip())
            except:
                raise UIException("The ID must be an integer")

            name = self.name_entry.get().strip()
            phone_number = self.phone_number_entry.get().strip()

            self.__person_service.add(_id, name, phone_number)
            messagebox.showinfo(message = "The person was added successfully!")

        except Exception as error:
            messagebox.showerror(message = error)


    def remove_person_gui(self):

        try:
            try:
                _id = int(self.id_entry.get().strip())
            except:
                raise UIException("The ID must be an integer")

            self.__person_service.remove(_id)
            messagebox.showinfo(message = "Person removed successfully!")


        except Exception as error:
            messagebox.showerror(message = error)


    def update_person_gui(self):

        try:
            try:
                _id = int(self.id_entry.get().strip())
            except:
                raise UIException("The ID must be an integer")

            new_name = self.name_entry.get().strip() or None
            new_phone_number = self.phone_number_entry.get().strip() or None

            self.__person_service.update(_id, new_name, new_phone_number)
            messagebox.showinfo(message = "Person updated successfully!")


        except Exception as error:
            messagebox.showerror(message=error)


    def lsit_people_gui(self):

        people = self.__person_service.get_people()

        if len(people) == 0:
            messagebox.showwarning(message = "No people found!")
        else:
            people_string = ""

            people.sort(key = lambda _person: _person.id)

            for person in people:
                people_string += f"{person}\n\n"

            messagebox.showinfo(message = people_string)



    def activities_button_pressed(self):
        activity_window = Toplevel(self.main_window)

        button = Button(activity_window, text="Add", command=self.add_activity_gui, height=2, width=10, font=font.Font(family='Time New Roman', size = 14)).grid(column = 0, row = 0)
        button = Button(activity_window, text="Remove", command=self.remove_activity_gui, height=2, width=10, font=font.Font(family='Time New Roman', size = 14)).grid(column = 0, row = 1)
        button = Button(activity_window, text="Update", command=self.update_activity_gui, height=2, width=10, font=font.Font(family='Time New Roman', size = 14)).grid(column = 0, row = 2)
        button = Button(activity_window, text="List", command=self.list_activities_gui, height=2, width=10, font=font.Font(family='Time New Roman', size = 14)).grid(column = 0, row = 3)


        Label(activity_window, text="ID:", font=font.Font(family='Time New Roman', size = 12)).grid(column = 1, row = 0)
        self.activity_id_entry = Entry(activity_window, {}, font=font.Font(family='Time New Roman', size = 12))
        self.activity_id_entry.grid(column = 2, row = 0)


        Label(activity_window, text="Date Year:", font=font.Font(family='Time New Roman', size = 12)).grid(column = 1, row = 1)
        self.date_year_entry = Entry(activity_window, {}, font=font.Font(family='Time New Roman', size = 12))
        self.date_year_entry.grid(column = 2, row = 1)

        Label(activity_window, text="Date Month:", font=font.Font(family='Time New Roman', size=12)).grid(column=1, row=2)
        self.date_month_entry = Entry(activity_window, {}, font=font.Font(family='Time New Roman', size=12))
        self.date_month_entry.grid(column = 2, row = 2)

        Label(activity_window, text="Date Day:", font=font.Font(family='Time New Roman', size=12)).grid(column=1, row=3)
        self.date_day_entry = Entry(activity_window, {}, font=font.Font(family='Time New Roman', size=12))
        self.date_day_entry.grid(column = 2, row = 3)

        Label(activity_window, text="Time Hour:", font=font.Font(family='Time New Roman', size = 12)).grid(column = 1, row = 4)
        self.time_hour_entry = Entry(activity_window, {}, font=font.Font(family='Time New Roman', size = 12))
        self.time_hour_entry.grid(column = 2, row = 4)

        Label(activity_window, text="Time Minute:", font=font.Font(family='Time New Roman', size=12)).grid(column=1, row=5)
        self.time_minute_entry = Entry(activity_window, {}, font=font.Font(family='Time New Roman', size=12))
        self.time_minute_entry.grid(column=2, row=5)

        Label(activity_window, text="People:", font=font.Font(family='Time New Roman', size = 12)).grid(column = 1, row = 6)
        self.people_entry = Entry(activity_window, {}, font=font.Font(family='Time New Roman', size = 12))
        self.people_entry.grid(column=2, row = 6)


        Label(activity_window, text="Description", font=font.Font(family='Time New Roman', size = 12)).grid(column = 1, row = 7)
        self.description_entry = Entry(activity_window, {}, font=font.Font(family='Time New Roman', size = 12))
        self.description_entry.grid(column=2, row=7)



    def add_activity_gui(self):
        try:
            try:
                _id = int(self.activity_id_entry.get().strip())
            except:
                raise UIException("The ID must be an integer")

            try:
                day = int(self.date_day_entry.get().strip())
                month = int(self.date_month_entry.get().strip())
                year = int(self.date_year_entry.get().strip())

                _date = datetime.date(year, month, day)
            except:
                raise UIException("Invalid date")

            try:
                hour = int(self.time_hour_entry.get().strip())
                minute = int(self.time_minute_entry.get().strip())

                _time = datetime.time(hour, minute, 0)
            except:
                raise UIException("Invalid time")

            description = self.description_entry.get().strip()

            people = self.people_entry.get().strip()
            people = people.split(", ")
            int_people = []

            for person_id in people:
                person_id = person_id.strip()

                if person_id == '':
                    continue

                if isinstance(person_id, int):
                    raise UIException(f"Person ID must be an integer ({person_id})")

                person_id = int(person_id)

                if not self.__person_service.person_exists(person_id):
                    raise RepositoryException(f"No people found with ID = {person_id}")

                int_people.append(int(person_id))

            self.__activity_service.add(_id, int_people, _date, _time, description)

            messagebox.showinfo(message = "The activity was added successfully!")
        except ValueError as error:
            messagebox.showerror(message = error)
        except Exception as error:
            messagebox.showerror(message = error)


    def remove_activity_gui(self):

        try:
            try:
                _id = int(self.activity_id_entry.get().strip())
            except:
                raise UIException("The ID must be an integer")

            self.__activity_service.remove(activity_id=_id)
            messagebox.showinfo(message = "Activity removed successfully")
        except Exception as error:
            messagebox.showerror(message = error)


    def update_activity_gui(self):

        try:
            try:
                _id = int(self.activity_id_entry.get().strip())
            except:
                raise UIException("The ID must be an integer")

            new_people = None
            new_date = None
            new_time = None

            if self.date_day_entry.get().strip() != '' and self.date_month_entry.get().strip() != '' and self.date_year_entry.get().strip() != '':
                try:
                    day = int(self.date_day_entry.get().strip())
                    month = int(self.date_month_entry.get().strip())
                    year = int(self.date_year_entry.get().strip())

                    new_date = datetime.date(year, month, day)
                except:
                    raise UIException("Invalid date")


            if self.time_hour_entry.get().strip() != '' and self.time_minute_entry.get().strip() != '':

                try:
                    hour = int(self.time_hour_entry.get().strip())
                    minute = int(self.time_minute_entry.get().strip())

                    new_time = datetime.time(hour, minute, 0)
                except:
                    raise UIException("Invalid time")


            new_description = self.description_entry.get().strip() if self.description_entry.get().strip() != '' else None

            if self.people_entry.get().strip() != '':
                people = self.people_entry.get().strip()
                people = people.split(", ")
                int_people = []

                for person_id in people:

                    if person_id == '':
                        continue

                    if isinstance(person_id, int):
                        raise UIException(f"Person ID must be an integer ({person_id})")

                    person_id = int(person_id)

                    if not self.__person_service.person_exists(person_id):
                        raise RepositoryException(f"No people found with ID = {person_id}")

                    int_people.append(int(person_id))

                new_people = int_people

            self.__activity_service.update(_id, new_people, new_date, new_time, new_description)
            messagebox.showinfo(message = " Activity updated successfully!")
        except Exception as error:
            messagebox.showerror(message = error)


    def list_activities_gui(self):

        activities = self.__activity_service.get_activities()

        if len(activities) == 0:
            messagebox.showwarning(message = "No activities found!")
        else:
            activities.sort(key=lambda _activity: _activity.id)
            activities_string = ""
            for activity in activities:
                activities_string += f"{activity}\n\n"

            messagebox.showinfo(message = activities_string)


    def search_button_pressed(self):

        window = Toplevel(self.main_window)

        button = Button(window, text="Date", command=self.search_by_date, height=2, width=10, font=font.Font(family='Time New Roman', size=14))
        button.grid(column=0, row=1)
        button = Button(window, text="Description", command=self.search_by_descritipn, height=2, width=10, font=font.Font(family='Time New Roman', size=14))
        button.grid(column=1, row=1)
        button = Button(window, text="Time", command=self.search_by_time, height=2, width=10, font=font.Font(family='Time New Roman', size=14))
        button.grid(column=2, row=1)
        button = Button(window, text="Name", command=self.search_by_name_gui, height=2, width=10, font=font.Font(family='Time New Roman', size=14))
        button.grid(column=0, row=2, columnspan = 2)
        button = Button(window, text="Number", command=self.search_by_phone_number, height=2, width=10, font=font.Font(family='Time New Roman', size=14))
        button.grid(column=1, row=2, columnspan = 2)

        Label(window, text="Text to search:", font=font.Font(family='Time New Roman', size=12)).grid(column=0, row=0)
        self.search_entry = Entry(window, {}, font=font.Font(family='Time New Roman', size=14))
        self.search_entry.grid(column=1, row=0, columnspan = 2)


    def search_by_name_gui(self):
        try:
            option = "name"
            people = self.__person_service.search_people(self.search_entry.get().strip(), option)
            self.show_list(people, "Mo matches found!")
        except Exception as error:
            messagebox.showerror(message = error)


    def search_by_phone_number(self):
        try:
            option = "number"
            people = self.__person_service.search_people(self.search_entry.get().strip(), option)
            self.show_list(people, "Mo matches found!")
        except Exception as error:
            messagebox.showerror(message=error)


    def search_by_date(self):
        try:
            option = "date"
            activities = self.__activity_service.search_activity(self.search_entry.get().strip(), option)
            self.show_list(activities, "Mo matches found!")
        except Exception as error:
            messagebox.showerror(message=error)

    def search_by_time(self):
        try:
            option = "time"
            activities = self.__activity_service.search_activity(self.search_entry.get().strip(), option)
            self.show_list(activities, "Mo matches found!")
        except Exception as error:
            messagebox.showerror(message=error)

    def search_by_descritipn(self):
        try:
            option = "description"
            activities = self.__activity_service.search_activity(self.search_entry.get().strip(), option)
            self.show_list(activities, "Mo matches found!")
        except Exception as error:
            messagebox.showerror(message=error)


    def show_list(self, _list, empty_message):

        if len(_list) == 0:
            messagebox.showwarning(message = empty_message)
        else:
            string = ""

            for oject in _list:
                string += f"{oject}\n\n"

            messagebox.showinfo(message = string)


    def statistics_button_pressed(self):

        window = Toplevel(self.main_window)

        button = Button(window, text="Given Date", command=self.statistics_given_date, height=2, width=10, font=font.Font(family='Time New Roman', size=14))
        button.grid(column=0, row=4)
        button = Button(window, text="Given Person", command=self.statistics_given_person, height=2, width=10, font=font.Font(family='Time New Roman', size=14))
        button.grid(column=1, row=4)
        button = Button(window, text="Busiest days", command=self.statistics_busiest_days, height=2, width=10, font=font.Font(family='Time New Roman', size=14))
        button.grid(column=2, row=4)

        Label(window, text="Date year:", font=font.Font(family='Time New Roman', size=12)).grid(column=0, row=0)
        self.statistics_year = Entry(window, {}, font=font.Font(family='Time New Roman', size=12))
        self.statistics_year.grid(column=1, row=0, columnspan=2)

        Label(window, text="Date month:", font=font.Font(family='Time New Roman', size=12)).grid(column=0, row=1)
        self.statistics_month = Entry(window, {}, font=font.Font(family='Time New Roman', size=12))
        self.statistics_month.grid(column=1, row=1, columnspan=2)

        Label(window, text="Date day:", font=font.Font(family='Time New Roman', size=12)).grid(column=0, row=2)
        self.statistics_day = Entry(window, {}, font=font.Font(family='Time New Roman', size=12))
        self.statistics_day.grid(column=1, row=2, columnspan=2)

        Label(window, text="Person ID:", font=font.Font(family='Time New Roman', size=12)).grid(column=0, row=3)
        self.statistics_id  = Entry(window, {}, font=font.Font(family='Time New Roman', size=12))
        self.statistics_id.grid(column=1, row=3, columnspan=2)


    def statistics_given_date(self):
        try:

            try:
                day = int(self.statistics_day.get().strip())
                month = int(self.statistics_month.get().strip())
                year = int(self.statistics_year.get().strip())

                date = datetime.date(year, month, day)
            except:
                raise UIException("Invalid date")

            activities = self.__activity_service.get_activities_by_date(date)
            self.show_list(activities, "No activities found!")

        except Exception as error:
            messagebox.showerror(message = error)


    def statistics_given_person(self):

        try:
            try:
                _id = int(self.statistics_id.get().strip())
            except:
                raise UIException("The ID must be an integer")

            activities = self.__activity_service.get_activities_with_person(_id)
            self.show_list(activities, "No activities found!")
        except Exception as error:
            messagebox.showerror(message=error)


    def statistics_busiest_days(self):

        dates = self.__activity_service.get_busiest_days()
        string = ""
        for date in dates:
            if date[1] == 1:
                string += f"Date: {date[0]} has {date[1]} activity\n"
            else:
                string += f"Date: {date[0]} has {date[1]} activities\n"

        if string == "":
            messagebox.showerror(message = "No activities found!")
        else:
            messagebox.showinfo(message = string)


    def undo_button_pressed(self):
        try:
            self.__undo_service.undo()
            messagebox.showinfo(message = "Successfully undid")
        except UndoException as ue:
            messagebox.showwarning(message = ue)
        except Exception as error:
            messagebox.showerror(message = error)

    def redo_button_pressed(self):
        try:
            self.__undo_service.redo()
            messagebox.showinfo(message = "Successfully redid")
        except UndoException as ue:
            messagebox.showwarning(message=ue)
        except Exception as error:
            messagebox.showerror(message = error)

