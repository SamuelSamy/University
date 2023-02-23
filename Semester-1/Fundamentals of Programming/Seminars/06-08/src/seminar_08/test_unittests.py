import unittest   # The test framework

class Test_TestIncrementDecrement(unittest.TestCase):
    def test_increment(self):
        self.assertEqual(4, 4)

    def test_decrement(self):
        self.assertEqual(3, 4)

class Test_TestIncrementDecrement2(unittest.TestCase):
    def test_increment(self):
        self.assertEqual(4, 4)

    def test_decrement(self):
        self.assertEqual(3, 4)

class Test_TestIncrementDecrement3(unittest.TestCase):
    def test_increment(self):
        self.assertEqual(4, 4)

    def test_decrement(self):
        self.assertEqual(3, 4)


if __name__ == '__main__':
    unittest.main()
