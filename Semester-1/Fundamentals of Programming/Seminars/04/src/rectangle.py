def get_radius(circle):
    return circle['radius']


def get_x(rectangle):
    return rectangle['x']


def get_y(rectangle):
    return rectangle['y']


def get_width(rectangle):
    return rectangle['width']


def get_height(rectangle):
    return rectangle['height']

    
def create_rectangle(x, y, width, height):

    if width < 1 or height < 1:
        raise ValueError("Can not create circle with radius < 1")
    
    return {
        "x": x,
        "y": y,
        "width": width,
        "height": height
    }

