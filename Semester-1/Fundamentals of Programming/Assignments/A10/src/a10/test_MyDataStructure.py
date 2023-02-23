import unittest
from types import FunctionType

from src.a10.MyDataStructure import MyDataStructure


class TestMyDataStructure(unittest.TestCase):

    def test_init(self):

        testing_value = 5
        with self.assertRaises(TypeError) as te:
            collection = MyDataStructure(testing_value)

        self.assertEqual(str(te.exception), f"TypeError: collection expected {type([])}, got {type(testing_value)}")


        collection = MyDataStructure([1, 4, 7, 8, 10])


    def test_iter(self):

        collection = MyDataStructure([1, 4, 7, 8, 10])

        expected = [1, 4, 7, 8, 10]
        result = []

        for item in collection:
            result.append(item)

        self.assertEqual(expected, result)


    def test_set(self):
        collection = MyDataStructure([1, 4, 7, 8, 10])
        collection[0] = 5
        self.assertEqual(collection, [5, 4, 7, 8, 10])


    def test_get(self):
        collection = MyDataStructure([1, 4, 7, 8, 10])
        self.assertEqual(collection[1], 4)


    def test_str(self):
        collection = MyDataStructure([1, 4, 7, 8, 10])
        self.assertEqual(str(collection), "[1, 4, 7, 8, 10]")


    def test_add(self):
        collection = MyDataStructure([1])
        collection.add(2)
        self.assertEqual([1, 2], collection)


    def test_remove(self):
        collection = MyDataStructure([1, 2])
        collection.remove(2)
        self.assertEqual([1], collection)


    def test_delitem(self):
        collection = MyDataStructure([1, 4, 7, 8, 10])
        del collection[0]
        self.assertEqual(collection, [4, 7, 8, 10])

    def test_sorted(self):

        collection = MyDataStructure([1, 4, 7, 8, 10])

        expected = sorted([1, 4, 7, 8, 10])
        result = MyDataStructure.sorted(collection)

        self.assertEqual(expected, result)


    def test_sort(self):

        def sort_function(a, b):
            return a[0] < b[0] or a[0] == b[0] and a[1] < b[1]

        collection = MyDataStructure([(0, 0), (1, 0), (0, 5), (1, 5)])
        collection = MyDataStructure.sorted(collection, compare_function = sort_function)
        self.assertEqual([(0, 0), (0, 5), (1, 0), (1, 5)], collection)

        testing_value = 5
        with self.assertRaises(TypeError) as te:
            collection = MyDataStructure.sorted(collection, testing_value)
        self.assertEqual(str(te.exception), f"TypeError: compare_function expected {FunctionType}, got {type(testing_value)}")

        testing_value = 5
        with self.assertRaises(TypeError) as te:
            collection = MyDataStructure.sorted(collection, sort_function, testing_value)
        self.assertEqual(str(te.exception), f"TypeError: reverse expected {type(True)}, got {type(testing_value)}")


    def test_filter(self):

        def validator(element):
            return element[0] == 0

        collection = MyDataStructure([(0, 0), (1, 0), (0, 5), (1, 5)])
        collection = MyDataStructure.filter(collection, validator)
        self.assertEqual([(1, 0), (1, 5)], collection)


    def test_eq(self):
        collection = MyDataStructure([(0, 0), (1, 0), (0, 5), (1, 5)])
        value = (5 == collection)
        self.assertEqual(value, False)
