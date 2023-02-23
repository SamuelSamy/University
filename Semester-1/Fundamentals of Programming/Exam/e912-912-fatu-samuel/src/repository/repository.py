class Repository:

    def __init__(self):
        self._year = 1
        self._new_people = 0
        self._population = 100
        self._land = 1000
        self._harvest = 3
        self._rats = True
        self._rats_ate = 200
        self._land_price = 20
        self._starved = 0

        self._grain_stock = 2800


    def get_rats_ate(self):
        return self._rats_ate


    def set_rats_ate(self, new_value):
        self._rats_ate = new_value


    def get_year(self):
        return self._year


    def get_new_people(self):
        return self._new_people


    def set_new_people(self, new_people):
        self._new_people = new_people


    def get_population(self):
        return self._population


    def get_land(self):
        return self._land


    def get_harvest(self):
        return self._harvest


    def set_harvest(self, new_value):
        self._harvest = new_value


    def get_rats(self):
        return self._rats


    def set_rats(self, new_value):
        self._rats = new_value


    def get_land_price(self):
        return self._land_price


    def increment_year(self):
        self._year += 1


    def update_land(self, acres):
        self._land = acres


    def set_land_price(self, new_price):
        self._land_price = new_price


    def update_grain_stock(self, new_value):
        self._grain_stock = new_value


    def set_starved(self, new_value):
        self._starved = new_value


    def get_starved(self):
        return self._starved


    def get_grain_stocks(self):
        return self._grain_stock

