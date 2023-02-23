from random import randint
from domain.students import Student

class Service:

    def __init__(self):
        self.__students = []
        self.__operations = []
    
    
    @property
    def students(self):
        self.__students.sort(key = lambda x: x.id, reverse = False)
        return self.__students
    

    @property
    def operations(self):
        return self.__operations


    def create_student(self, id, name, group):
        """
        Creats a `student` object

        Args:
            id (int): Student's ID
            name (string): Student's name
            group (int): Student's group


        Returns:
            Student: A student object with the specified parameters
        """
        return Student(id, name, group)


    def get_student(self, index):
        """
        Returns the index-th element from the students list

        Args:
            index (int): Element's index

        Returns:
            Student: A `Student` object
        """
        return self.students[index]


    def add_student(self, student, operation_index = None):
        """
        Appends `student` to the `students` list

        Args:
            student (Student): A `Student`object
            operation_index (int, optional): Index of the operation. Defaults to None.

        Raises:
            ValueError: If there is already a student with the same ID
        """

        if self.same_id_exists(student):
            raise ValueError("A student with the same ID already exists")

        self.students.append(student)

        if operation_index is not None:
            self.__save_operation("remove", operation_index, student)


    def same_id_exists(self, student):
        """
        Checks if `student.id` already exists (is not unique)

        Args:
            student (Student): 

        Returns:
            [type]: [description]
        """
        id = student.id

        for stud in self.students:
            if stud.id == id:
                return True

        return False


    def remove_student(self, student, operation_index = None):
        """
        Removes the `student` object from the list of students

        Args:
            student (Student): A 'student' object
            operation_index (int, optional): Index of the operation. Defaults to None.
        """
        self.students.remove(student)
        
        if operation_index is not None: #if operation_index:
            self.__save_operation("add", operation_index, student)
                

    def generate_students(self):

        no = 10

        names = [
            "Liam", "Olivia", "Noah", "Emma", "Oliver", "Ava", "Elijah", "Charlotte", "William", "Sophia", 
            "James", "Amelia", "Benjamin", "Isabella", "Lucas", "Mia", "Henry", "Evelyn", "Alexander", "Harper"
        ]

        while no > 0:
            
            id = randint(1, 100)
            name = names[randint(0, len(names) - 1)]
            group = randint(911, 917)

            student = self.create_student(id, name, group)

            if not self.same_id_exists(student):
                self.add_student(student)
                no -= 1
    

    def filter_students(self, group, operation_index):
        """
        Removes all students from a given group

        Args:
            group (int): The group student's will be removed from
            operation_index (int): Index of the operation. Defaults to None.

        Raises:
            ValueError: if `group` is not an integer
            ValueError: if there are no students in the given group
        """
        try:
            group = int(group)
        except:
            raise ValueError("`group` must be an integer")

        i = 0
        starting_len = len(self.students)

        while i < len(self.students):
            student = self.get_student(i)

            if student.group == group:
                self.remove_student(student, operation_index)
                i -= 1

            i += 1
        

        if len(self.students) == starting_len:
            raise ValueError("There are not students in this group")


    def __save_operation(self, command, index, student):
        """
        Stores the operation into a list

        Args:
            command (string): The command word
            index (int): The index of the operation
            student (Student): A student object
        """

        operation = self.__create_operation(command, index, student)
        self.__operations.append(operation)
    

    def __create_operation(self, command, index, student):
        """
        Creates an operation dictionary

        Args:
            command (string): The command word
            index (int): The index of the operation
            student (Student): A student object

        Raises:
            ValueError: if `command` is not in ['add', 'remove']

        Returns:
            dictionary: A dictionary representing the operation
        """                

        if command not in ["add", "remove"]:
            raise ValueError("`command` must be 'add' or 'remove'")

        return {
            "index": index,
            "command": command,
            "student": student
        }

    
    def __get_operation(self, index):
        """
        Returns the index-th element from `self.operations`

        Args:
            index (int): The index of the specific student

        Returns:
            Student: A student object
        """
        return self.operations[index]


    def __get_operation_index(self, opeartion):
        """
        Get the index of an operation

        Args:
            opeartion (dict): A dictionary representing an operation

        Returns:
            int: The index of the operation
        """
        return opeartion['index']


    def __get_operation_command(self, opeartion):
        """
        Get the command word of an operation

        Args:
            opeartion (dict): A dictionary representing an operation

        Returns:
            string: A string representing the command word ('add' / 'remove')
        """
        return opeartion['command']


    def __get_operation_student(self, opeartion):
        """
        Get the student object of an operation

        Args:
            opeartion (dict): A dictionary representing an operation

        Returns:
            Student: A student object
        """
        return opeartion['student']


    def undo_last_operation(self, index):
        """
        Reverts the last operation

        Args:
            index (int): The current operation index (last operation index is `index - 1`)
        """

        last_operation_index = index - 1

        while len(self.operations) > 0 and self.__get_operation_index(self.__get_operation(-1)) == last_operation_index:

            operation = self.__get_operation(-1)
            student = self.__get_operation_student(operation)


            if self.__get_operation_command(operation) == 'add':
                self.add_student(student)
            else:
                self.remove_student(student)
            
            del self.operations[-1]

        index -= 1


def test_create_student():
    
    serv = Service()

    student = serv.create_student(1, "Sam", 912)

    assert student.id == 1
    assert student.name == "Sam"
    assert student.group == 912

    try:
        s = serv.create_student("test", "Sam", 912)
        assert False
    except ValueError as ve:
        assert str(ve) == "`id` must be an integer"

    try:
        s = serv.create_student(2, "Sam", -912)
        assert False
    except ValueError as ve:
        assert str(ve) == "`group` must be a possitive integer"


    try:
        s = serv.create_student(2, "Sam", "test")
        assert False
    except ValueError as ve:
        assert str(ve) == "`group` must be a possitive integer"


def test_add_student():
    
    serv = Service()

    serv.add_student(Student(1, "Sam", 1))

    assert len(serv.students) == 1
    
    student = serv.get_student(0)

    assert student.id == 1
    assert student.name == "Sam"
    assert student.group == 1  

    try:
        serv.add_student(Student(1, "Sam", 1))
        assert False
    except ValueError as ve:
        assert str(ve) == "A student with the same ID already exists"


def test_get_student():

    serv = Service()

    serv.add_student(Student(1, "Sam", 1))
    serv.add_student(Student(2, "Dan", 1))
    serv.add_student(Student(3, "Andi", 1))

    student = serv.get_student(0)

    assert student.id == 1
    assert student.name == "Sam"
    assert student.group == 1


def test_same_id_exists():
    
    serv = Service()

    serv.add_student(Student(1, "Sam", 1))

    student1 = Student(1, "Dan", 1)
    student2 = Student(2, "Andi", 1)

    assert serv.same_id_exists(student1) == True
    assert serv.same_id_exists(student2) == False


def test_remove_student():
    
    serv = Service()

    serv.add_student(Student(2, "Dan", 1))
    serv.add_student(Student(3, "Andi", 1))

    student = serv.get_student(0)

    serv.remove_student(student)

    assert len(serv.students) == 1
    
    student = serv.get_student(0)

    assert student.id == 3
    assert student.name == "Andi"
    assert student.group == 1  


def test_services():

    test_create_student()
    test_get_student()
    test_add_student()
    test_same_id_exists()
    test_remove_student()



test_services()