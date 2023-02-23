class MyDataStructure:

    class MyIterator:

        def __init__(self, collection, starting_position, increment_value):
            self._collection = collection
            self._pos = starting_position
            self._inc = increment_value

        def __next__(self):

            if self._pos == len(self._collection):
                raise StopIteration()

            self._pos += self._inc
            return self._collection[self._pos - self._inc]


    def __init__(self, collection = None):

        if collection is not None and not isinstance(collection, type([])):
            raise TypeError(f"TypeError: collection expected {type([])}, got {type(collection)}")

        self.__data = collection if collection is not None else []
        self._pos = 0


    def __iter__(self):
        return self.MyIterator(self.__data, 0, 1)


    def __len__(self):
        return len(self.__data)


    def __setitem__(self, key, value):
        self.__data[key] = value


    def __getitem__(self, key):
        return self.__data[key]


    def __delitem__(self, key):
        del self.__data[key]


    def __str__(self):
        return str(self.__data)


    def add(self, element):
        self.__data.append(element)


    def remove(self, element):
        return self.__data.remove(element)


    def __eq__(self, value):
        if not isinstance(value, list):
            return False

        return value == self.__data


    @staticmethod
    def sorted(_list, compare_function = None, reverse = False):
        from types import FunctionType

        """
        Gnome sort
        
        Args:
            _list: An iterable object
            compare_function (function, optional): A function on which the sorting will be based on. Defaults to `object0 < object1`.
            reverse (bool, optional): True if the elements will be sorted in a reversed order. Defaults to False

        Raises:
            TypeError: If `compare_function` is not a function
            TypeError: If reverse is not a boolean

        Returns:
            MyDataStructure: An ordered `MyDataStructure`
        """

        if compare_function is not None and not isinstance(compare_function, FunctionType):
            raise TypeError(f"TypeError: compare_function expected {FunctionType}, got {type(compare_function)}")

        if reverse is not None and reverse not in [True, False]:
            raise TypeError(f"TypeError: reverse expected {type(True)}, got {type(reverse)}")

        sorted_list = MyDataStructure()

        for element in _list:
            sorted_list.add(element)

        def default_compare_function(_object0, _object1):
            return _object0 < _object1

        if compare_function is None:
            compare_function = default_compare_function

        index = 0

        while index < len(sorted_list):

            if index == 0 or (not reverse and not compare_function(sorted_list[index], sorted_list[index - 1])) or (reverse and compare_function(sorted_list[index], sorted_list[index - 1])):
                index += 1

            else:
                sorted_list[index], sorted_list[index - 1] = sorted_list[index - 1], sorted_list[index]
                index -= 1

        return sorted_list


    @staticmethod
    def filter(_list, validator):
        """Removes every element that does not pass the `validator`

        Args:
            _list: An iterable object
            validator (function): A filter function
        """
        filtered_list = MyDataStructure()

        for element in _list:
            if not validator(element):
                filtered_list.add(element)

        return filtered_list
