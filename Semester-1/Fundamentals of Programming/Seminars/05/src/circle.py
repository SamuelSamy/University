import random
import rectangle

def get_center(circle):
    return circle['x'], circle['y']


def get_radius(circle):
    return circle['radius']


def get_x(circle):
    return circle['x']


def get_y(circle):
    return circle['y']


def create_circle(x, y, radius):

    if radius < 1:
        raise ValueError("Can not create circle with radius < 1")
    
    return {
        "x": x,
        "y": y,
        "radius": radius
    }


def add_circle(circles_list, circle):
    """
    Appends a circle to the list, if it is distinct

    Args:
        circles_list [lsit of dictionaries]: The list of circles
        circle [dictionary]: A circle

    Returns:
        [bool]:     -> True if the circle is  distinct, False otherwise
    """

    for circ in circles_list:
        if get_center(circ) == get_center(circle):
            return False

    circles_list.append(circle)
    return True


def generate_circles(circles_list, n):
    """
    Generates `n` circles and appends them to `circles_list`

    Args:
        circles_list (list of dictionaries): The list we are going to append the new circles
        n (int): The number of circles that will be generated 
    """
    
    added = 0

    while added < n:
        radius = random.randint(1, 15)
        x = random.randint(1 + radius, 39 - radius)
        y = random.randint(1 + radius, 39 - radius)

        if add_circle(circles_list, create_circle(x, y, radius)):
            added += 1
    

def delete_circle(circles_list, circle):
    circles_list.remove(circle)


def delete_enclosed_circles(circles_list, rectangle):
    """
    Deletes all circles from `circles_list` that are enclosed in `rectangle`

    Args:
        circles_list (list of dictionaries): A list with our current circles
        rectangle ([dictionary]): A rectangle

    Returns:
        int: The amount of circles deleted
    """

    deleted_circles_count = 0

    index = 0

    while index < len(circles_list):
        circ = circles_list[index]

        if enclosed_in_rectangle(circ, rectangle):
            delete_circle(circles_list, circ)
            deleted_circles_count += 1
        else:
            index += 1

    return deleted_circles_count


def enclosed_in_rectangle(circle, rect):
    """
    Checks if `circle` is enclosed in `rect`

    Args:
        circle (dictionary): A circle dictionary
        rect (dictionary): A rectangle dictionary

    Returns:
        bool: True if the circle is enclosed in the rectangle, False otherwise
    """

    if get_x(circle) + get_radius(circle) > rectangle.get_x(rect) + rectangle.get_width(rect):
        return False
    
    if get_x(circle) - get_radius(circle) < rectangle.get_x(rect):
        return False

    if get_y(circle) + get_radius(circle) > rectangle.get_y(rect) + rectangle.get_height(rect):
        return False

    if get_y(circle) - get_radius(circle) < rectangle.get_y(rect):
        return False

    return True


def test_enclosed_in_rectangle():

    assert enclosed_in_rectangle(
        circle = create_circle(5, 5, 1),
        rect = rectangle.create_rectangle(0, 0, 40, 40)
    ) == True

    assert enclosed_in_rectangle(
        circle = create_circle(1, 1, 10),
        rect = rectangle.create_rectangle(0, 0, 40, 40)
    ) == False

    assert enclosed_in_rectangle(
        circle = create_circle(1, 1, 1),
        rect = rectangle.create_rectangle(0, 0, 40, 40)
    ) == True

    assert enclosed_in_rectangle(
        circle = create_circle(-5, -5, 1),
        rect = rectangle.create_rectangle(0, 0, 40, 40)
    ) == False

    assert enclosed_in_rectangle(
        circle = create_circle(24, 24, 15),
        rect = rectangle.create_rectangle(0, 0, 40, 40)
    ) == True


def sort_circles(circles_list):
    """
    Copies the provided list into a new one, and returns the sorted list

    Args:
        circles_list (list of disctionaries): A list of circles

    Returns:
        list of disctionaries: The sorted list
    """

    sorted_circles = circles_list.copy()
    
    sorted_circles = sorted(sorted_circles, key = lambda circ: circ['radius'], reverse = True)

    return sorted_circles
