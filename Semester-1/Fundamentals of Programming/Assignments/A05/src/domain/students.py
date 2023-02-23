
class Student:

    def __init__(self, id, name, group):
        """
        Creats a `student` object

        Args:
            id (int): Student's ID
            name (string): Student's name
            group (int): Student's group

        Raises:
            ValueError: if `id` is not an integer 
            ValueError: if `group` is not an intger or is less than 0
        """

        try:
            id = int(id)
        except:
            raise ValueError("`id` must be an integer")

        try:
            group = int(group)
            assert group >= 0
        except:
            raise ValueError("`group` must be a possitive integer")


        self.__id = id
        self.__name = name
        self.__group = group    
        
           
    @property
    def id(self):
        return self.__id

    
    @property
    def name(self):
        return self.__name

    
    @property
    def group(self):
        return self.__group

    
    def __str__(self):
        return (f"Student ID: {self.__id}  |  Name: {self.__name}  |  Group: {self.__group}")


def test_student():
    
    student = Student(1, "Sam", 912)

    assert student.id == 1
    assert student.name == "Sam"
    assert student.group == 912

    try:
        s = Student("test", "Sam", 912)
        assert False
    except ValueError as ve:
        assert str(ve) == "`id` must be an integer"

    try:
        s = Student(2, "Sam", -912)
        assert False
    except ValueError as ve:
        assert str(ve) == "`group` must be a possitive integer"


    try:
        s = Student(2, "Sam", "test")
        assert False
    except ValueError as ve:
        assert str(ve) == "`group` must be a possitive integer"


test_student()