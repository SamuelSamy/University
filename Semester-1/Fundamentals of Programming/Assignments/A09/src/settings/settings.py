from jproperties import Properties


class Settings:

    def __init__(self):

        config = Properties()

        with open("settings/settings.properties", "rb") as config_file:
            config.load(config_file)

        self.__repository = config.get('repository').data
        self.__people = config.get('people').data
        self.__activities = config.get('activities').data
        self.__ui = config.get('ui').data


    @property
    def repository(self):
        return self.__repository


    @property
    def people(self):
        return self.__people

    @property
    def activities(self):
        return self.__activities


    @property
    def ui(self):
        return self.__ui
