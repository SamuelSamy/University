"""
Seminar 3
    -> add Exceptions
    -> implement command-based user interface
        add 0,0,1           # add circle at (0,0) and radius 1
        delete 0,0; 1,1     # delete circles at center (0,0) and (1,1)
        list                # display the list of circles
        exit
    -> add unit tests
"""

import math

"""
    Non-UI function
        - no print, no inout statements
"""


# circle functions
# {'x' : 1, 'y' : 2, 'rad': 3 } - corner at (1, 2) at radius 3
# We want to hide the circle's representation from the outside world (hide complexity)

def create_circle(x, y, radius):
    """
    Create a new circle
    :param x: The X coord of the circle center
    :param y: The Y coord of the circle center
    :param radius: Circle radius, must be a positive integer
    :return: A newly created circle, None if circle could not be created
    :except: ValueError if radius < 1
    """

    if radius < 1:
        raise ValueError("Can not create circle with radius < 1")

    return {
        'x': x,
        'y': y,
        'rad': radius
    }


# Write getters / setters for circle center and radius

def get_center(circle):
    return circle['x'], circle['y']


def get_radius(circle):
    return circle['rad']


def build_tangent_circles(circle_list, circle):
    """
    Return a list of the circles tangent with the given one
    :param circle_list: The list of all circles
    :param circle: Our circle
    :return: A list of all circles that are tangent with the given one
    """

    result = []

    for c in circle_list:
        if c is not circle and circles_are_tangent(c, circle):
            result.append(c)

    return result


def circles_are_tangent(circle_1, circle_2):
    """
    Determines whether the two circles are tangent
    :param circle_1: First circle
    :param circle_2: Second circle
    :return: True if they are tangent, False otherwise
    """

    c1 = get_center(circle_1)
    c2 = get_center(circle_2)

    r1 = get_radius(circle_1)
    r2 = get_radius(circle_2)

    center_distance = math.sqrt((c1[0] - c2[0]) ** 2 + (c1[1] - c2[1]) ** 2)

    return center_distance == r1 + r2

# Test function is named test_<function that is tested>
def test_circles_are_tangent():
    # assert crashes if the expression is False
    assert circles_are_tangent(create_circle(0, 0, 1), create_circle(2, 0, 1)) == True
    assert circles_are_tangent(create_circle(0, 0, 1), create_circle(1, 0, 1)) == False
    assert circles_are_tangent(create_circle(0, 0, 5), create_circle(4, 0, 1)) == False

# Non-circle functions


def add_circle(circle_list, circle):
    """
    Adds a new circle to the list
    :param circle_list: The list of circles
    :param circle: The new circle
    :except: ValueError if duplicate circle centers
    """
    for circ in circle_list:
        if get_center(circ) == get_center(circle):
            raise ValueError("Duplicated circle centers")

    circle_list.append(circle)


def init_circles():
    """
    Create a few circles to have at program startup
    :return:
    """

    return [create_circle(1, 1, 1), create_circle(1, 2, 3), create_circle(2, 1, 7), create_circle(5, 3, 2)]
    

def split_command(command):
    """
    Divide user input into command word and command parameters
    
    """
    
    aux= command.split(" ", maxsplit = 1)

    command_word = aux[0]
    command_param = aux[1] if len(aux) == 2 else None

    return command_word, command_param

"""
    UI functions are here
        - program talks to the user via print / input statements
"""

def add_circle_command(circle_list, param):
    new_circles = param.split("; ")

    for circle in new_circles:
        x, y, rad = circle.split(",")

        try:
            add_circle(circle_list, create_circle(int(x), int(y), int(rad)))
        except ValueError as ve:
            print(f"Circle at center ({x},{y}) and radius {rad} could not be added\n{ve}")


def show_circles(circle_list):
    index = 1
    for circle in circle_list:
        print(f"{index}. center at {get_center(circle)} radius of {get_radius(circle)}")
        index += 1


def show_circles_intersect_ui(circle_list):

    show_circles(circle_list)

    # TODO Crash if entered value can not be converted to int

    index = int(input("Select circle: "))

    circle = circle_list[index - 1]

    tangent_circles = build_tangent_circles(circle_list, circle)

    show_circles(tangent_circles)


def add_circle_ui(circle_list):
    # each circle is represented using a dict
    
    """
    RN, function combines C-style error codes (return None) with Python exceptions
    
        What are C-style error codes?
            -> return 0 means succes
            -> return 1 is an error, 2 is another error, etc
            -> Problem is that it is implicit => errors go under the radar if not checked
    

        Python exceptions
            -> Explicit => you have to raise Exception & catch Exception
            -> untreared Exception crashes the program 
    """

    is_error = True

    while is_error:
        try:
            circle_x = int(input("Enter X = "))
            circle_y = int(input("Enter Y = "))
            circle_rad = int(input("Enter radius = "))

            is_error = False
        except ValueError as ve:
            print("Invalid input value. Please try again")
        
    try:
        circle = create_circle(circle_x, circle_y, circle_rad)
        add_circle(circle_list, circle)
    except ValueError as ve:
        print(str(ve))



def print_menu():
    print("1. Add circle")
    print("3. Show circles")
    print("4. Show circles that intersect a given one")
    print("5. Exit")



def start_menu():

    circle_list = init_circles()

    while True:

        print_menu()
        option = input("Enter option ")

        try:

            if option == "1":
                add_circle_ui(circle_list)
            elif option == "3":
                show_circles(circle_list)
            elif option == "4":
                show_circles_intersect_ui(circle_list)
            elif option == "5":
                break
            else:
                print("Invalid option")
        except ValueError as ve:
            print(str(ve))


def start_command():
    """
    add 0,0,1           # add circle at (0,0) and radius 1
    delete 0,0; 1,1     # delete circles at center (0,0) and (1,1)
    list                # display the list of circles
    exit
    """
    circle_list = init_circles()
    
    while True:
        command = input("promt> ")

        command_word, command_param = split_command(command)
        try:
            if command_word == "list":
                show_circles(circle_list)
            elif command_word == "add":
                add_circle_command(circle_list, command_param)
            elif command_word == "exit":
                return
            else:
                print("Bad command")
        except ValueError as ve:
            print(str(ve))


# start_menu()
start_command()